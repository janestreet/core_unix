@@ portable

open! Core
open! Import
open Basement.Primitives

type t = Unix.file_descr [@@deriving compare ~localize, equal ~localize, sexp]

include Equal.S with type t := t
include Hashable.S_plain with type t := t
include Stringable.S with type t := t

val of_int : int -> t
val of_int_exn : int -> t
val to_int : t -> int
val to_i32 : t -> Int32_u.t [@@zero_alloc]
val of_i32 : Int32_u.t -> t [@@zero_alloc]
