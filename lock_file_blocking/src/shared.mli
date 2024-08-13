open! Core

val random : Random.State.t Lazy.t
val read_file_and_convert : of_string:(string -> 'a) -> string -> 'a option

val repeat_with_timeout
  :  ?max_retry_delay:Core.Core_private.Span_float.t
  -> ?random:Base.Random.State.t lazy_t
  -> ?timeout:Core.Core_private.Span_float.t
  -> (string -> 'a)
  -> string
  -> 'a
