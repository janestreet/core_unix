open! Core
open! Import
open Core.Filename

let create_arg_type ?key of_string =
  Core.Command.Arg_type.create ?key of_string ~complete:(fun _ ~part ->
    let completions =
      (* `compgen -f` handles some fiddly things nicely, e.g. completing "foo" and
         "foo/" appropriately. *)
      let command = sprintf "bash -c 'compgen -f %s'" part in
      let chan_in = Unix.open_process_in command in
      let completions = In_channel.input_lines chan_in in
      ignore (Unix.close_process_in chan_in);
      List.map (List.sort ~compare:String.compare completions) ~f:(fun comp ->
        match Sys_unix.is_directory comp with
        | `Yes -> comp ^ "/"
        | `No | `Unknown -> comp)
    in
    match completions with
    | [ dir ] when String.is_suffix dir ~suffix:"/" ->
      (* If the only match is a directory, we fake out bash here by creating a bogus
         entry, which the user will never see - it forces bash to push the completion
         out to the slash. Then when the user hits tab again, they will be at the end
         of the line, at the directory with a slash and completion will continue into
         the subdirectory.
      *)
      [ dir; dir ^ "x" ]
    | _ -> completions)
;;

let arg_type = create_arg_type Fn.id

external realpath : string -> string = "core_unix_realpath"

(* We want [random_letter ()] to be thread-safe.

   This is thread safe because [Stdlib.Random.State.int] is (the only updates to
   the state are effected via [caml_lxm_next_unboxed] in the OCaml runtime, which
   cannot be interrupted by an OCaml thread context switch).
*)
let random_letter =
  let prng_key = Domain.DLS.new_key Stdlib.Random.State.make_self_init in
  let letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  fun () ->
    let prng = Domain.DLS.get prng_key in
    letters.[Stdlib.Random.State.int prng (String.length letters)]
;;

let retry ~in_dir ~prefix ~suffix f =
  let in_dir = Option.value in_dir ~default:temp_dir_name in
  let escape s =
    String.map s ~f:(function
      | '/' | '\'' | '\000' | '\n' | '-' -> '_'
      | c -> c)
  in
  let prefix = escape prefix in
  let suffix = escape suffix in
  let rec try_name ~attempts =
    let name =
      let rnd = String.init 6 ~f:(fun _ -> random_letter ()) in
      sprintf "%s.tmp.%s%s" prefix rnd suffix
    in
    let name = concat in_dir name in
    try f name with
    | Unix.Unix_error (EINTR, _, _) -> try_name ~attempts
    | Unix.Unix_error (EEXIST, _, _) when Int.O.(attempts > 0) ->
      try_name ~attempts:(attempts - 1)
  in
  (* number of attempts taken from glibc:
     https://elixir.bootlin.com/glibc/glibc-2.38/source/sysdeps/posix/tempname.c#L34
  *)
  try_name ~attempts:238328
;;

let temp_dir ?(perm = 0o700) ?in_dir prefix suffix =
  retry ~in_dir ~prefix ~suffix (fun name ->
    UnixLabels.mkdir name ~perm;
    name)
;;

let open_temp_file_fd ?(close_on_exec = false) ?(perm = 0o600) ?in_dir prefix suffix =
  let mode : Unix.open_flag list = [ O_EXCL; O_CREAT; O_RDWR ] in
  let mode : Unix.open_flag list = if close_on_exec then O_CLOEXEC :: mode else mode in
  retry ~in_dir ~prefix ~suffix (fun name -> name, UnixLabels.openfile ~perm ~mode name)
;;

let temp_file ?(perm = 0o600) ?in_dir prefix suffix =
  retry ~in_dir ~prefix ~suffix (fun name ->
    let fd = UnixLabels.openfile ~perm ~mode:[ O_CLOEXEC; O_EXCL; O_CREAT ] name in
    (* On Linux and many other Unix implementations, the file descriptor is guaranteed to be
       closed after calling [close]. *)
    match Unix.close fd with
    | () | (exception (_ : exn)) -> name)
;;

let open_temp_file ?(close_on_exec = true) ?perm ?in_dir prefix suffix =
  let name, fd = open_temp_file_fd ~close_on_exec ?perm ?in_dir prefix suffix in
  let out = Unix.out_channel_of_descr fd in
  name, out
;;
