(* Calls [Lock_file_blocking.create ~unlink_on_exit:true] such that it takes the lock and
   then fails to write the lock file, and exits normally afterwards. With
   [open-other-file], it first opens another file, which gets the descriptor number that
   [create] released, and writes to it without flushing. *)
open! Core
module Unix = Core_unix

let () =
  let dir, open_other_file =
    match Sys.get_argv () with
    | [| _; dir |] -> dir, false
    | [| _; dir; "open-other-file" |] -> dir, true
    | _ -> failwith "usage: create_fails_with_unlink_on_exit.exe DIR [open-other-file]"
  in
  (* With a file size limit of 0, writing the pid into the lock file fails with EFBIG. *)
  Signal.Expert.set Signal.xfsz Ignore;
  let file_size_limit = Unix.RLimit.get Unix.RLimit.file_size in
  Unix.RLimit.set Unix.RLimit.file_size { file_size_limit with cur = Limit 0L };
  let result =
    match Lock_file_blocking.create ~unlink_on_exit:true (dir ^/ "lock") with
    | (_ : bool) -> "create returned"
    | exception Unix.Unix_error (EFBIG, _, _) -> "create raised EFBIG"
  in
  Unix.RLimit.set Unix.RLimit.file_size file_size_limit;
  print_endline result;
  if open_other_file
  then (
    (* Flushed by [exit], after the [at_exit] handlers have run. *)
    let other_file = Out_channel.create (dir ^/ "other-file") in
    Out_channel.output_string other_file "other data\n")
;;
