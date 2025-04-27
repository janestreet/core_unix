open! Core
open! Import

include module type of struct
  include Time_float
end

(** [of_tm] converts a [Unix.tm] (mirroring a [struct tm] from the C stdlib) into a
    [Time.t]. Note that the [tm_wday], [tm_yday], and [tm_isdst] fields are ignored. *)
val of_tm : Unix.tm -> zone:Zone.t -> t

(** [pause span] sleeps for span time. *)
val pause : Span.t -> unit

(** [interruptible_pause span] sleeps for span time unless interrupted (e.g. by delivery
    of a signal), in which case the remaining unslept portion of time is returned. *)
val interruptible_pause : Span.t -> [ `Ok | `Remaining of Span.t ]

(** [pause_forever] sleeps indefinitely. *)
val pause_forever : unit -> never_returns

(** [format t fmt] formats the given time according to fmt, which follows the formatting
    rules given in 'man strftime'. The time is output in the given timezone. Here are some
    commonly used control codes:

    {v
      %Y - year (4 digits)
      %y - year (2 digits)
      %m - month
      %d - day
      %H - hour
      %M - minute
      %S - second
    v}

    a common choice would be: %Y-%m-%d %H:%M:%S

    Although %Z and %z are interpreted as format strings, neither are correct in the
    current implementation. %Z always refers to the local machine timezone, and does not
    correctly detect whether DST is active. The effective local timezone can be controlled
    by setting the "TZ" environment variable before calling [format]. %z behaves
    unreliably and should be avoided.

    Not all strftime control codes are standard; the supported subset will depend on the C
    libraries linked into a given executable. *)
val format : t -> string -> zone:Zone.t -> string

(** [parse string ~fmt ~zone] parses [string], according to [fmt], which follows the
    formatting rules given in 'man strptime'. The time is assumed to be in the given
    timezone.

    {v
      %Y - year (4 digits)
      %y - year (2 digits)
      %m - month
      %d - day
      %H - hour
      %M - minute
      %S - second
    v}

    Raise if [allow_trailing_input] is false and [fmt] does not consume all of the input. *)
val parse
  :  ?allow_trailing_input:bool (** default = false *)
  -> string
  -> fmt:string
  -> zone:Zone.t
  -> t
