open! Core
open! Import
module Mutex = Error_checking_mutex
module Queue = Linked_queue

(** Synchronized queue type *)
type 'a t =
  { ev_q : 'a Queue.t
  ; maxsize : int
  ; mutex : (Mutex.t[@sexp.opaque])
  ; not_empty : (Condition.t[@sexp.opaque])
  ; not_full : (Condition.t[@sexp.opaque])
  }
[@@deriving sexp_of]

let create maxsize =
  let ev_q = Queue.create () in
  let mutex = Mutex.create () in
  let not_empty = Condition.create () in
  let not_full = Condition.create () in
  { ev_q; mutex; not_empty; not_full; maxsize }
;;

let finally t =
  let len = Queue.length t.ev_q in
  if len <> 0 then Condition.signal t.not_empty;
  if len < t.maxsize then Condition.signal t.not_full;
  Mutex.unlock t.mutex
;;

let wrap q run =
  Mutex.lock q.mutex;
  Exn.protectx ~f:run q ~finally
;;

let clear q =
  let run q = Queue.clear q.ev_q in
  wrap q run
;;

let wait_not_full q =
  while Queue.length q.ev_q >= q.maxsize do
    Condition.wait q.not_full q.mutex
  done
;;

let wait_not_empty q =
  while Queue.is_empty q.ev_q do
    Condition.wait q.not_empty q.mutex
  done
;;

(** Pushes an event on the queue if there's room *)
let push q x =
  let run q =
    wait_not_full q;
    Queue.enqueue q.ev_q x
  in
  wrap q run
;;

(** Pushes an event on the queue, unconditionally, may grow the queue past maxsize *)
let push_uncond q x =
  let run q = Queue.enqueue q.ev_q x in
  wrap q run
;;

(** Pushes an event on the queue if the queue is less than maxsize, otherwise drops it.
    Returns true if the push was successful *)
let push_or_drop q x =
  let run q =
    if Queue.length q.ev_q < q.maxsize
    then (
      Queue.enqueue q.ev_q x;
      true)
    else false
  in
  wrap q run
;;

(** computes the length of the queue *)
let length q =
  let run q = Queue.length q.ev_q in
  wrap q run
;;

(** Pops an event off of the queue, blocking until something is available *)
let pop q =
  let run q =
    wait_not_empty q;
    Queue.dequeue_exn q.ev_q
  in
  wrap q run
;;

(** Pops an event off of the queue, blocking until something is available. Returns pair of
    the element found and the length of remaining queue *)
let lpop q =
  let run q =
    wait_not_empty q;
    let el = Queue.dequeue_exn q.ev_q in
    let len = Queue.length q.ev_q in
    el, len
  in
  wrap q run
;;

let transfer_queue_in_uncond q in_q =
  if not (Queue.is_empty in_q)
  then (
    let run q = Queue.transfer ~src:in_q ~dst:q.ev_q in
    wrap q run)
;;

let transfer_queue_in q in_q =
  if not (Queue.is_empty in_q)
  then (
    let run q =
      wait_not_full q;
      Queue.transfer ~src:in_q ~dst:q.ev_q
    in
    wrap q run)
;;

let transfer_queue_nowait_nolock sq q = Queue.transfer ~src:sq.ev_q ~dst:q

let transfer_queue_nowait sq q =
  if not (Queue.is_empty sq.ev_q)
  then (
    let run sq = transfer_queue_nowait_nolock sq q in
    wrap sq run)
;;

let transfer_queue sq q =
  let run sq =
    wait_not_empty sq;
    transfer_queue_nowait_nolock sq q
  in
  wrap sq run
;;

(* The external version of wait_not_empty *)
let wait_not_empty sq =
  let run sq = wait_not_empty sq in
  wrap sq run
;;
