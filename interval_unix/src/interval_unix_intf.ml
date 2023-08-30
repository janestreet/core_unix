open! Core
module Interval = Interval_lib.Interval
module Zone = Time_float.Zone

module type S_time = sig
  module Time : sig
    type t

    module Ofday : sig
      type t
    end
  end

  include Interval.S with type bound = Time.t (** @open *)

  (** [create_ending_after ?zone (od1, od2) ~now] returns the smallest interval [(t1 t2)]
      with minimum [t2] such that [t2 >= now], [to_ofday t1 = od1], and [to_ofday t2 =
      od2]. If a zone is specified, it is used to translate [od1] and [od2] into times,
      otherwise the machine's time zone is used.

      It is not guaranteed that the interval will contain [now]: for instance if it's
      11:15am, [od1] is 12pm, and [od2] is 2pm, the returned interval will be 12pm-2pm
      today, which obviously doesn't include 11:15am. In general [contains (t1 t2) now]
      will only be true when now is between [to_ofday od1] and [to_ofday od2].

      You might want to use this function if, for example, there's a daily meeting from
      10:30am-11:30am and you want to find the next instance of the meeting, relative to
      now. *)
  val create_ending_after : ?zone:Zone.t -> Time.Ofday.t * Time.Ofday.t -> now:Time.t -> t

  (** [create_ending_before ?zone (od1, od2) ~ubound] returns the smallest interval [(t1
      t2)] with maximum [t2] such that [t2 <= ubound], [to_ofday t1 = od1], and [to_ofday
      t2 = od2]. If a zone is specified, it is used to translate [od1] and [od2] into
      times, otherwise the machine's time zone is used.

      You might want to use this function if, for example, there's a lunch hour from
      noon to 1pm and you want to find the first instance of that lunch hour (an interval)
      before [ubound]. The result will either be on the same day as [ubound], if
      [to_ofday ubound] is after 1pm, or the day before, if [to_ofday ubound] is any
      earlier. *)
  val create_ending_before
    :  ?zone:Zone.t
    -> Time.Ofday.t * Time.Ofday.t
    -> ubound:Time.t
    -> t
end

module type Interval_unix = sig
  (**
     [S_time] is a signature that's used below to define the interfaces for [Time] and
     [Time_ns] without duplication.
  *)
  module type S_time = S_time

  (** {3 Specialized time interval types} *)

  module Time : S_time with module Time := Time_float and type t = Time_float.t Interval.t
  module Time_ns : S_time with module Time := Time_ns and type t = Time_ns.t Interval.t

  (**
     [Stable] is used to build stable protocols. It ensures backwards compatibility by
     checking the sexp and bin-io representations of a given module. Here it's applied
     to the [Time], and [Time_ns] intervals.
  *)
  module Stable : sig
    module V1 : sig
      module Time : Stable with type t = Time.t
      module Time_ns : Stable with type t = Time_ns.t
    end
  end
end
