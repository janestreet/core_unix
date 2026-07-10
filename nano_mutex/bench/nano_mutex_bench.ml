open Core

module type Mutex = sig
  type t

  val create : unit -> t
  val lock : t -> unit
  val unlock : t -> unit
end

module Bench_mutex (M : Mutex) = struct
  let%bench "create" = ignore (M.create () : M.t)

  let%bench_fun "lock/unlock" =
    let l = M.create () in
    fun () ->
      M.lock l;
      M.unlock l
  ;;
end

module Nano_mutex : Mutex = struct
  include Nano_mutex

  let lock = lock_exn
  let unlock t = unlock_exn t
end

module%bench [@name "Caml_threads.Mutex"] _ = Bench_mutex (Caml_threads.Mutex)
module%bench [@name "Error_checking_mutex"] _ = Bench_mutex (Error_checking_mutex)
module%bench [@name "Nano_mutex"] _ = Bench_mutex (Nano_mutex)
