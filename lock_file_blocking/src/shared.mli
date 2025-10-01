open! Core

val random : Random.State.t Lazy.t
val read_file_and_convert : of_string:(string -> 'a) -> string -> 'a option

(** Exceptions raised by the function are not caught. The exception returned by `Retriable
    is raised if the timeout is reached. *)
val repeat_with_timeout
  :  ?max_retry_delay:Core.Core_private.Span_float.t
  -> ?random:Base.Random.State.t lazy_t
  -> ?timeout:Core.Core_private.Span_float.t
  -> (string -> ('a, [ `Retriable of exn ]) Result.t)
  -> string
  -> 'a
