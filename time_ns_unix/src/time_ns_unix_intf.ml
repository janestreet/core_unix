open! Core
module Time = Time_float_unix

module type Option = sig
  include Immediate_option.S_int63
  include Identifiable with type t := t
end

module type Span = sig
  include module type of struct
      include Time_ns.Span
    end [@ocaml.remove_aliases]
    with module Private := Time_ns.Span.Private
end

module type Ofday = sig
  include module type of struct
    include Time_ns.Ofday
  end

  val arg_type : t Core.Command.Arg_type.t
  val now : zone:Time.Zone.t -> t

  val to_ofday : t -> Time.Ofday.t
    [@@deprecated
      "[since 2019-01] use [to_ofday_float_round_nearest] or \
       [to_ofday_float_round_nearest_microsecond]"]

  val of_ofday : Time.Ofday.t -> t
    [@@deprecated
      "[since 2019-01] use [of_ofday_float_round_nearest] or \
       [of_ofday_float_round_nearest_microsecond]"]

  val to_ofday_float_round_nearest : t -> Time.Ofday.t
  val to_ofday_float_round_nearest_microsecond : t -> Time.Ofday.t
  val of_ofday_float_round_nearest : Time.Ofday.t -> t
  val of_ofday_float_round_nearest_microsecond : Time.Ofday.t -> t

  module Zoned : sig
    (** Sexps look like "(12:01 nyc)"

        Two [t]'s may or may not correspond to the same times depending on which date
        they're evaluated. *)
    type t [@@deriving bin_io, sexp, hash]

    include Pretty_printer.S with type t := t

    (** Strings look like "12:01 nyc" *)
    include Stringable with type t := t

    val arg_type : t Core.Command.Arg_type.t
    val create : Time_ns.Ofday.t -> Time.Zone.t -> t
    val create_local : Time_ns.Ofday.t -> t
    val ofday : t -> Time_ns.Ofday.t
    val zone : t -> Time.Zone.t
    val to_time_ns : t -> Date.t -> Time_ns.t

    module With_nonchronological_compare : sig
      (** It is possible to consistently compare [t]'s, but due to the complexities of
          time zones and daylight savings, the resulting ordering is not chronological.
          That is, [compare t1 t2 > 0] does not imply [t2] occurs after [t1] every day,
          or any day. *)
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end

    module Stable : sig
      module V1 : sig
        type nonrec t = t [@@deriving hash]

        include Stable_without_comparator_with_witness with type t := t
      end
    end
  end

  module Option : sig
    include Option with type value := t
    include Quickcheck.S with type t := t
    include Diffable.S_atomic with type t := t

    module Stable : sig
      module V1 : sig
        include Stable_int63able_with_witness with type t = t
        include Diffable.S_atomic with type t := t
      end
    end

    (** Returns [some] if the given span is a valid time since start of day, and [none]
        otherwise. *)
    val of_span_since_start_of_day : Time_ns.Span.t -> t
  end
end

(** An absolute point in time, more efficient and precise than the [float]-based {!Time},
    but representing a narrower range of times.

    This module represents absolute times with nanosecond precision, approximately between
    the years 1823 and 2116 CE.

    Some reasons you might prefer [Time_ns.t] over float-based [Time.t]:

    - It has superior performance.

    - It uses [int]s rather than [float]s internally, which makes certain things easier to
      reason about, since [int]s respect a bunch of arithmetic identities that [float]s
      don't, e.g., [x + (y + z) = (x + y) + z].

    Some reasons you might prefer to use float-based [Time] instead of this module:

    - Some libraries use [Time.t] values, often for historical reasons, so it may be
      necessary to use [Time.t] with them.

    - [Time_ns] silently ignores overflow.

    Neither {!Time_ns_unix} nor {!Time_float_unix} are available in JavaScript, but both
    {!Core.Time_ns} and {!Core.Time} are.

    See {!Core.Time_ns} for additional low level documentation. *)
module type Time_ns_unix = sig
  include module type of struct
      include Time_ns
    end [@ocaml.remove_aliases]
    with module Span := Time_ns.Span
    with module Ofday := Time_ns.Ofday
    with module Stable := Time_ns.Stable
    with module Option := Time_ns.Option

  module Span : Span

  val arg_type : t Core.Command.Arg_type.t

  (** [Option.t] is like [t option], except that the value is immediate.  This module
      should mainly be used to avoid allocations. *)
  module Option : sig
    include Option with type t = Time_ns.Option.t with type value := t
    include Quickcheck.S with type t := t
    include Diffable.S_atomic with type t := t

    module Stable : sig
      module V1 : sig
        include Stable_int63able_with_witness with type t = t
        include Diffable.S_atomic with type t := t
      end
    end
  end

  (** See {!Time.Ofday}. *)
  module Ofday : Ofday

  (** String conversions use the local timezone by default. Sexp conversions use
      [get_sexp_zone ()] by default, which can be overridden by calling [set_sexp_zone].
      These default time zones are used when writing a time, and when reading a time with
      no explicit zone or UTC offset.

      Sexps and strings display the date, ofday, and UTC offset of [t] relative to the
      appropriate time zone. *)
  include Identifiable with type t := t

  include Diffable.S_atomic with type t := t

  include sig
      type t [@@deriving sexp_grammar]
    end
    with type t := t

  module Zone : module type of Time.Zone with type t = Time.Zone.t

  (** These functions are identical to those in [Time] and get/set the same variable. *)

  val get_sexp_zone : unit -> Zone.t
  val set_sexp_zone : Zone.t -> unit

  (** [t_of_sexp_abs sexp] as [t_of_sexp], but demands that [sexp] indicate the timezone
      the time is expressed in. *)
  val t_of_sexp_abs : Sexp.t -> t

  val sexp_of_t_abs : t -> zone:Zone.t -> Sexp.t

  (** Conversion functions that involved Ofday.Zoned.t, exactly analogous to the
      conversion functions that involve Ofday.t *)
  val of_date_ofday_zoned : Date.t -> Ofday.Zoned.t -> t

  val to_date_ofday_zoned : t -> zone:Time.Zone.t -> Date.t * Ofday.Zoned.t
  val to_ofday_zoned : t -> zone:Time.Zone.t -> Ofday.Zoned.t
  val to_string_fix_proto : [ `Utc | `Local ] -> t -> string
  val of_string_fix_proto : [ `Utc | `Local ] -> string -> t

  (** This is like [of_string] except that if the string doesn't specify the zone then it
      raises rather than assume the local timezone. *)
  val of_string_abs : string -> t

  (** [of_string_gen ~if_no_timezone s] attempts to parse [s] to a [t].  If [s] doesn't
      supply a time zone [if_no_timezone] is consulted. *)
  val of_string_gen
    :  if_no_timezone:[ `Fail | `Local | `Use_this_one of Zone.t ]
    -> string
    -> t

  (** [pause span] sleeps for [span] time. *)
  val pause : Span.t -> unit

  (** [interruptible_pause span] sleeps for [span] time unless interrupted (e.g. by
      delivery of a signal), in which case the remaining unslept portion of time is
      returned. *)
  val interruptible_pause : Span.t -> [ `Ok | `Remaining of Span.t ]

  (** [pause_forever] sleeps indefinitely. *)
  val pause_forever : unit -> never_returns

  (** [format t fmt] formats the given time according to fmt, which follows the formatting
      rules given in 'man strftime'.  The time is output in the given timezone. Here are
      some commonly used control codes:

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
      correctly detect whether DST is active. The effective local timezone can be
      controlled by setting the "TZ" environment variable before calling [format]. %z
      behaves unreliably and should be avoided.

      Not all strftime control codes are standard; the supported subset will depend on the
      C libraries linked into a given executable.
  *)
  val format : t -> string -> zone:Zone.t -> string

  (** [parse string ~fmt ~zone] parses [string], according to [fmt], which follows the
      formatting rules given in 'man strptime'.  The time is assumed to be in the given
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

      Raise if [allow_trailing_input] is false and [fmt] does not consume all of the
      input. *)
  val parse
    :  ?allow_trailing_input:bool (** default = false *)
    -> string
    -> fmt:string
    -> zone:Zone.t
    -> t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving equal, hash]

      include
        Stable_int63able_with_witness
          with type t := t
           and type comparator_witness = comparator_witness

      include
        Comparable.Stable.V1.With_stable_witness.S
          with type comparable := t
          with type comparator_witness := comparator_witness

      include Diffable.S_atomic with type t := t
    end

    (** Provides a sexp representation that is independent of the time zone of the machine
        writing it. *)
    module Alternate_sexp : sig
      module V1 : sig
        include Stable_without_comparator with type t = t
        include Diffable.S_atomic with type t := t
      end
    end

    module Option : sig
      module V1 : sig
        include Stable_int63able_with_witness with type t = Option.t
        include Diffable.S_atomic with type t := t
      end
    end

    module Span : sig
      module V1 : sig
        type t = Span.t [@@deriving hash, equal, sexp_grammar]

        include Stable_int63able_with_witness with type t := t
        include Diffable.S_atomic with type t := t
      end

      module V2 : sig
        type t = Span.t [@@deriving hash, equal, sexp_grammar]
        type nonrec comparator_witness = Span.comparator_witness

        include
          Stable_int63able_with_witness
            with type t := t
            with type comparator_witness := comparator_witness

        include
          Comparable.Stable.V1.With_stable_witness.S
            with type comparable := t
            with type comparator_witness := comparator_witness

        include Stringable.S with type t := t
        include Diffable.S_atomic with type t := t
      end

      module Option : sig
        module V1 : sig
          include Stable_int63able_with_witness with type t = Span.Option.t
          include Diffable.S_atomic with type t := t
        end

        module V2 : sig
          include Stable_int63able_with_witness with type t = Span.Option.t
          include Diffable.S_atomic with type t := t
        end
      end
    end

    module Ofday : sig
      module V1 : sig
        type t = Ofday.t [@@deriving equal, hash, sexp_grammar]

        include
          Stable_int63able_with_witness
            with type t := Ofday.t
             and type comparator_witness = Time_ns.Stable.Ofday.V1.comparator_witness

        include Diffable.S_atomic with type t := t
      end

      module Zoned : sig
        module V1 : sig
          type nonrec t = Ofday.Zoned.t [@@deriving hash]

          include Stable_without_comparator_with_witness with type t := t
        end
      end

      module Option : sig
        module V1 : sig
          include Stable_int63able_with_witness with type t = Ofday.Option.t
          include Diffable.S_atomic with type t := t
        end
      end
    end

    module Zone = Timezone.Stable
  end
end
