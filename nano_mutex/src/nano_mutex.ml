open! Core
open! Import

let ok_exn = Or_error.ok_exn

(* A [Blocker.t] is an ordinary mutex and conditional variable used to implement blocking
   when there is lock contention. *)
module Blocker : sig
  type t [@@deriving sexp_of]

  val create : unit -> t
  val critical_section : t -> f:(unit -> 'a) -> 'a
  val wait : t -> unit
  val signal : t -> unit
  val save_unused : t -> unit
end = struct
  (* Our use of mutexes is always via [Mutex.critical_section], so that we always lock
     them and unlock them from a single thread.  So, we use [Core.Mutex], which is
     error-checking mutexes, which will catch any use that is not what we expect. *)
  module Condition = Condition
  module Mutex = Error_checking_mutex

  type t =
    { mutex : (Mutex.t[@sexp.opaque])
    ; condition : (Condition.t[@sexp.opaque])
    }
  [@@deriving sexp_of]

  (* We keep a cache of unused blockers, since they are relatively costly to create, and
     we should never need very many simultaneously.  We should never need more blockers
     than the number of nano mutexes being simultaneously blocked on, which of course is
     no more than the total number of simultaneous threads. *)
  let unused : t Thread_safe_queue.t = Thread_safe_queue.create ()

  (* [save_unused t] should be called when [t] is no longer in use, so it can be returned
     by a future call of [create]. *)
  let save_unused t = Thread_safe_queue.enqueue unused t

  let create () =
    match Thread_safe_queue.dequeue unused with
    | Thread_safe_queue.Dequeue_result.Empty ->
      { mutex = Mutex.create (); condition = Condition.create () }
    | Not_empty { elt } -> elt
  ;;

  let critical_section t ~f = Mutex.critical_section t.mutex ~f
  let wait t = Condition.wait t.condition t.mutex
  let signal t = Condition.signal t.condition
end

module Thread_id_option : sig
  type t [@@deriving equal ~localize, sexp_of] [@@immediate]

  val none : t
  val some : int -> t
  val is_none : t -> bool
  val is_some : t -> bool
end = struct
  type t = int [@@deriving sexp_of]

  let none = -1
  let[@inline always] is_none t = t = none
  let[@inline always] is_some t = t <> none
  let[@inline always] some int = int
  let sexp_of_t t = if t = none then [%sexp "None"] else [%sexp (t : t)]

  (* The atomicity of some sections marked "BEGIN/END ATOMIC" later in this file require
     [equal] to be implemented such that the OCaml compiler will not insert safepoints in
     its prelude.  We write out the definition rather than deriving it for this reason. *)
  let%template[@inline] equal (t1 : int) t2 = t1 = t2 [@@mode m = (global, local)]
end

(* We represent a nano mutex using an OCaml record.  The [id_of_thread_holding_lock] field
   represents whether the mutex is locked or not, and if it is locked, which thread holds
   the lock.  We use [Thread_id_option] instead of [int option] for performance reasons
   (using [int option] slows down lock+unlock by a factor of almost two).

   The mutex record has an optional [blocker] field for use when the mutex is contended.
   We use the OS-level condition variable in [blocker] to [wait] in a thread that desires
   the lock and to [signal] from a thread that is releasing it.

   When thinking about the implementation, it is helpful to remember the following
   desiderata:

 * Safety -- only one thread can acquire the lock at a time.  This is accomplished
   usng a test-and-set to set [id_of_thread_holding_lock].

 * Liveness -- if the mutex is unlocked and some threads are waiting on it, then one of
   those threads will be woken up and given a chance to acquire it.  This is accomplished
   by only waiting when we can ensure that there will be a [signal] of the condition
   variable in the future.  See the more detailed comment in [lock].

 * Performance -- do not spin trying to acquire the lock.  This is accomplished by
   waiting on a condition variable if a lock is contended. *)
type t =
  { mutable id_of_thread_holding_lock : Thread_id_option.t
  ; mutable num_using_blocker : int
  ; mutable blocker : Blocker.t Uopt.t
  }
[@@deriving sexp_of]

let invariant t =
  try
    assert (t.num_using_blocker >= 0);
    (* It is the case that if [t.num_using_blocker = 0] then [Option.is_none t.blocker],
       however the converse does not necessarily hold.  The code in [with_blocker] doesn't
       take care to atomically increment [t.num_using_blocker] and set [t.blocker] to
       [Some].  It could, but doing so is not necessary for the correctness of of
       [with_blocker], which only relies on test-and-set of [t.blocker] to make sure
       there is an agreed-upon winner in the race to create a blocker. *)
    if t.num_using_blocker = 0 then assert (Uopt.is_none t.blocker)
  with
  | exn -> failwiths "invariant failed" (exn, t) [%sexp_of: exn * t]
;;

let equal (t : t) t' = phys_equal t t'

let create () =
  { id_of_thread_holding_lock = Thread_id_option.none
  ; num_using_blocker = 0
  ; blocker = Uopt.none
  }
;;

let is_locked t = Thread_id_option.is_some t.id_of_thread_holding_lock
let current_thread_id () = Thread.id (Thread.self ())

let current_thread_has_lock t =
  Thread_id_option.equal
    t.id_of_thread_holding_lock
    (current_thread_id () |> Thread_id_option.some)
;;

let[@cold] error_recursive_lock t =
  Error
    (Error.create
       "attempt to lock mutex by thread already holding it"
       (current_thread_id (), t)
       [%sexp_of: int * t])
;;

let try_lock t =
  (* The following code relies on an atomic test-and-set of [id_of_thread_holding_lock],
     so that there is a definitive winner in a race between multiple lockers and everybody
     agrees who acquired the lock. *)
  let current_thread_id = current_thread_id () |> Thread_id_option.some in
  (* BEGIN ATOMIC *)
  if Thread_id_option.is_none t.id_of_thread_holding_lock
  then (
    t.id_of_thread_holding_lock <- current_thread_id;
    (* END ATOMIC *)
    Ok `Acquired)
  else if Thread_id_option.equal current_thread_id t.id_of_thread_holding_lock
  then error_recursive_lock t
  else Ok `Not_acquired
;;

let try_lock_exn t = ok_exn (try_lock t)

(* Marked with attributes so the allocation of [new_blocker] at the call site cannot be
   sunk down into the atomic section (there exists no barrier in OCaml right now to
   prevent this) *)

let[@inline never] [@specialise never] [@local never] with_blocker0 t ~new_blocker =
  (* BEGIN ATOMIC *)
  if Uopt.is_some t.blocker
  then Uopt.unsafe_value t.blocker
  else (
    t.blocker <- Uopt.some new_blocker;
    new_blocker)
;;

(* END ATOMIC *)

(* [with_blocker t f] runs [f blocker] in a critical section.  It allocates a blocker for
   [t] if [t] doesn't already have one. *)
let with_blocker t f =
  t.num_using_blocker <- t.num_using_blocker + 1;
  let blocker =
    match%optional.Uopt t.blocker with
    | Some blocker -> blocker
    | None ->
      (* We allocate [new_blocker] here because one cannot allocate inside an atomic
         region. *)
      let new_blocker = Blocker.create () in
      let blocker =
        (* We need the following test-and-set to be atomic so that there is a definitive
           winner in a race between multiple calls to [with_blocker], so that everybody
           agrees what the underlying [blocker] is. *)
        with_blocker0 t ~new_blocker
      in
      if not (phys_equal blocker new_blocker) then Blocker.save_unused new_blocker;
      blocker
  in
  protect
    ~f:(fun () -> Blocker.critical_section blocker ~f:(fun () -> f blocker) [@nontail])
    ~finally:(fun () ->
      (* We need the following decrement-test-and-set to be atomic so that we're sure that
         the last user of blocker clears it. *)
      (* BEGIN ATOMIC *)
      t.num_using_blocker <- t.num_using_blocker - 1;
      if t.num_using_blocker = 0
      then (
        t.blocker <- Uopt.none;
        (* END ATOMIC *)
        Blocker.save_unused blocker)) [@nontail]
;;

let rec lock t =
  (* The following code relies on an atomic test-and-set of [id_of_thread_holding_lock],
     so that there is a definitive winner in a race between multiple [lock]ers, and
     everybody agrees who acquired the lock.

     If [is_locked t], we block the locking thread using [Blocker.wait], until some
     unlocking thread [Blocker.signal]s us.  There is a race between the [wait] and the
     [signal].  If the unlocking thread signals in between our test of
     [t.id_of_thread_holding_lock] and our [wait], then our [wait] could miss the signal
     and block forever.  We avoid this race by committing to waiting inside a
     [with_blocker], which increments [t.num_using_blocker].  If the [signal] occurs
     before the [with_blocker], then it will have cleared [t.id_of_thread_holding_lock],
     which we will notice as [not (is_locked t)], and then not [wait], and loop trying to
     [lock] again.  Otherwise, when an [unlock] occurs, it will see that [is_some
     t.blocker], and will enter a critical section on [blocker].  But then it must wait
     until our critical section on [blocker] finishes, and hence until our call to [wait]
     finishes.  Hence, the [signal] will occur after the [wait].

     The recursive call to [lock] will not spin.  It happens either because we just lost
     the race with an unlocker, in which case the subsequent [lock] will succeed, or
     we actually had to block because someone is holding the lock.  The latter is the
     overwhelmingly common case.

     Other threads can change [t.id_of_thread_holding_lock] concurrently with this code.
     However, no other thread can set it to our [current_thread_id], since threads only
     ever set [t.id_of_thread_holding_lock] to their current thread id, or clear it. *)
  let current_thread_id = current_thread_id () |> Thread_id_option.some in
  (* BEGIN ATOMIC *)
  if Thread_id_option.is_none t.id_of_thread_holding_lock
  then (
    t.id_of_thread_holding_lock <- current_thread_id;
    (* END ATOMIC *)
    Ok ())
  else if Thread_id_option.equal current_thread_id t.id_of_thread_holding_lock
  then error_recursive_lock t
  else (
    with_blocker t (fun blocker -> if is_locked t then Blocker.wait blocker);
    lock t)
;;

let lock_exn t = ok_exn (lock t)

type message =
  { current_thread_id : int
  ; mutex : t
  }
[@@deriving sexp_of]

let[@cold] error_attempt_to_unlock_mutex_held_by_another_thread t =
  Error
    (Error.create
       "attempt to unlock mutex held by another thread"
       { current_thread_id = current_thread_id (); mutex = t }
       [%sexp_of: message])
;;

let[@cold] error_attempt_to_unlock_an_unlocked_mutex t =
  Error
    (Error.create
       "attempt to unlock an unlocked mutex"
       { current_thread_id = current_thread_id (); mutex = t }
       [%sexp_of: message])
;;

let unlock t =
  let current_thread_id = current_thread_id () in
  (* We need the following test-and-set to be atomic so that there is a definitive
     winner in a race between multiple unlockers, so that one unlock succeeds and the
     rest fail. *)
  (* BEGIN ATOMIC *)
  if Thread_id_option.is_some t.id_of_thread_holding_lock
  then
    if Thread_id_option.equal
         t.id_of_thread_holding_lock
         (current_thread_id |> Thread_id_option.some)
    then (
      t.id_of_thread_holding_lock <- Thread_id_option.none;
      (* END ATOMIC *)
      if Uopt.is_some t.blocker then with_blocker t Blocker.signal;
      Ok ())
    else error_attempt_to_unlock_mutex_held_by_another_thread t
  else error_attempt_to_unlock_an_unlocked_mutex t
;;

let unlock_exn t = ok_exn (unlock t)

let critical_section t ~f =
  lock_exn t;
  protect ~f ~finally:(fun () -> unlock_exn t)
;;
