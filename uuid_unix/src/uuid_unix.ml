open! Core
module Unix = Core_unix

let gethostname = Portable_lazy.from_fun (fun () -> Unix.gethostname ())

let create () =
  Uuid.Private.create
    ~hostname:(Portable_lazy.force gethostname)
    ~pid:(Unix.getpid () |> Pid.to_int)
;;
