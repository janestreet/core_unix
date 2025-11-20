open! Core
open! Import

module M = struct
  type t = Unix.file_descr

  external to_int : t -> int @@ portable = "%identity"
  external of_int : int -> t @@ portable = "%identity"
  external of_int_exn : int -> t @@ portable = "%identity"

  let of_string string = of_int (Int.of_string string)
  let to_string t = Int.to_string (to_int t)
  let hash t = Int.hash (to_int t)

  let%template compare t1 t2 = (Int.compare [@mode m]) (to_int t1) (to_int t2)
  [@@mode m = (global, local)]
  ;;

  let t_of_sexp sexp = of_int (Int.t_of_sexp sexp)

  let sexp_of_t t =
    (* File descriptors 0, 1, 2 (stdin, stdout, stderr) are stable, so we show them even
       in test. *)
    match am_running_test && Int.( > ) (to_int t) 2 with
    | false -> [%sexp (to_int t : int)]
    | true -> [%sexp "_"]
  ;;
end

include M

include%template Hashable.Make_plain_and_derive_hash_fold_t [@mode portable] (M)

(* Given that [to_int] and [of_int] are set to "%identity", this is considerably more
   direct. It's unfortunate, but despite [Caml_unix] using [type t = int] in the
   implementation, [Unix.file_descr] is abstract and cannot be tagged [@@immediate]. *)
let%template equal t1 t2 = (Int.equal [@mode m]) (to_int t1) (to_int t2)
[@@mode m = (global, local)]
;;

external unbox_int32
  :  (int32[@local_opt])
  -> (int32#[@unboxed])
  @@ portable
  = "%unbox_int32"

external box_int32 : (int32#[@unboxed]) -> (int32[@local_opt]) @@ portable = "%box_int32"

let to_i32 t = unbox_int32 (Int.to_int32_trunc (to_int t))
let of_i32 t = of_int (Int.of_int32_trunc (box_int32 t))
