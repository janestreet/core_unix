open! Core
open! Import

type 'a t = int

[%%template
[@@@mode.default m = (global, local)]

module type S = Syscall_result_intf.S [@mode m] with type 'a syscall_result := 'a t
module type Arg = Syscall_result_intf.Arg [@mode m]]

let create_error err = -Unix_error.to_errno err
let is_ok t = Int.O.(t >= 0)
let is_error t = Int.O.(t < 0)

let error_code_exn t =
  if is_ok t
  then failwiths "Syscall_result.error_code_exn received success value" t [%sexp_of: int]
  else -t
;;

let error_exn t = Unix_error.of_errno (error_code_exn t)

module%template [@mode m = (global, local)] Make (M : Arg [@mode m]) () = struct
  (* The only reason to have one of these per functor invocation is to make it trivial to
     get the type right. *)
  let preallocated_errnos : (_, Unix_error.t) Result.t array =
    Array.init 64 ~f:(fun i -> Error (Unix_error.of_errno i))
  ;;

  (* Since we return [-errno] from C, we implicitly rely on there not being a 0 [errno].
     However, we have 0 in [preallocated_errnos], partly so we can index directly by
     [errno]. *)
  let%test "no 0 errno" =
    Poly.equal preallocated_errnos.(0) (Error (Unix_error.EUNKNOWNERR 0))
  ;;

  let num_preallocated_errnos = Array.length preallocated_errnos

  type nonrec t = M.t t

  let[@mode m = (global, m)] compare = (Int.compare [@mode m])
  let[@mode m = (global, m)] equal = (Int.equal [@mode m])

  let preallocated_ms =
    let rec loop i rev_acc =
      (* Preallocate at most a handful of Ms.  2048 is the first round binary number after
         1500, the likely maximum result for many network functions that use
         [Syscall_result.Int]. *)
      if i = 2048
      then Array.of_list_rev rev_acc
      else (
        match M.of_int_exn i with
        | exception _ -> Array.of_list_rev rev_acc
        | m -> loop (i + 1) (Ok m :: rev_acc))
    in
    loop 0 []
  ;;

  let num_preallocated_ms = Array.length preallocated_ms

  let create_ok x =
    let t = M.to_int x in
    if t < 0
    then failwithf "Syscall_result.create_ok received negative value (%d)" t ()
    else t
  ;;

  let create_error = create_error
  let is_ok = is_ok
  let is_error = is_error

  let to_result t =
    if is_ok t
    then
      if t < num_preallocated_ms
      then Array.unsafe_get preallocated_ms t
      else Ok (M.of_int_exn t)
    else (
      let errno = -t in
      if errno < num_preallocated_errnos
      then Array.unsafe_get preallocated_errnos errno
      else Error (Unix_error.of_errno errno))
  ;;

  let sexp_of_t t = [%sexp_of: (M.t, Unix_error.t) Result.t] (to_result t)

  let ok_exn t =
    if is_ok t
    then M.of_int_exn t
    else failwiths "Syscall_result.ok_exn received error value" t sexp_of_t
  ;;

  let error_code_exn t =
    if is_ok t
    then failwiths "Syscall_result.error_code_exn received success value" t sexp_of_t
    else -t
  ;;

  let error_exn t = Unix_error.of_errno (error_code_exn t)

  let reinterpret_error_exn t =
    if is_ok t
    then failwiths "Syscall_result.cast_error_exn received success value" t sexp_of_t
    else t
  ;;

  let ok_or_unix_error_exn t ~syscall_name =
    if is_ok t
    then M.of_int_exn t
    else raise (Unix.Unix_error (Unix_error.of_errno (-t), syscall_name, ""))
  ;;

  let ok_or_unix_error_with_args_exn t ~syscall_name x sexp_of_x =
    if is_ok t
    then M.of_int_exn t
    else
      raise
        (Unix.Unix_error
           (Unix_error.of_errno (-t), syscall_name, Sexp.to_string (sexp_of_x x)))
  ;;

  let is_none t = is_error t
  let unchecked_value t = M.of_int_exn t

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none = is_none
      let unsafe_value = unchecked_value
    end
  end

  module Private = struct
    let of_int t = t
    let length_preallocated_errnos = Array.length preallocated_errnos
    let length_preallocated_ms = Array.length preallocated_ms
  end
end

module Int = Make [@mode local] (Int) ()

module Unit =
  Make [@mode local]
    (struct
      type t = unit [@@deriving sexp_of, compare ~localize]

      let of_int_exn n = assert (n = 0)
      let to_int () = 0
    end)
    ()

module File_descr = Make [@mode local] (File_descr) ()

let unit = Unit.create_ok ()
let ignore_ok_value t = Core.Int.min t 0
