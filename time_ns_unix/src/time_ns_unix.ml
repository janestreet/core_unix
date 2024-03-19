open! Core
open! Int.Replace_polymorphic_compare
module Unix = Core_unix
module Time = Time_float_unix
include Time_ns
module Zone = Time.Zone
module Span = Time_ns.Span

let nanosleep t = Span.of_sec (Unix.nanosleep (Span.to_sec t))

let pause_for t =
  let time_remaining =
    (* If too large a float is passed in (Span.max_value for instance) then nanosleep
       will return immediately, leading to an infinite and expensive select loop.  This
       is handled by pausing for no longer than 100 days. *)
    nanosleep (Span.min t (Span.scale Span.day 100.))
  in
  if Span.( > ) time_remaining Span.zero then `Remaining time_remaining else `Ok
;;

(** Pause and don't allow events to interrupt. *)
let rec pause span =
  match pause_for span with
  | `Remaining span -> pause span
  | `Ok -> ()
;;

(** Pause but allow events to interrupt. *)
let interruptible_pause = pause_for

let rec pause_forever () =
  pause Span.day;
  pause_forever ()
;;

let to_string t = to_string_abs t ~zone:(Lazy.force Zone.local)

exception Time_string_not_absolute of string [@@deriving sexp]

let of_string_gen ~if_no_timezone s =
  let default_zone () =
    match if_no_timezone with
    | `Fail -> raise (Time_string_not_absolute s)
    | `Local -> Lazy.force Zone.local
    | `Use_this_one zone -> zone
  in
  of_string_gen ~default_zone ~find_zone:Zone.find_exn s
;;

let of_string_abs s = of_string_gen ~if_no_timezone:`Fail s
let of_string s = of_string_gen ~if_no_timezone:`Local s
let arg_type = Core.Command.Arg_type.create of_string_abs

let to_tm t ~zone : Unix.tm =
  let date, ofday = to_date_ofday t ~zone in
  let parts = Ofday.to_parts ofday in
  { tm_year = Date.year date - 1900
  ; tm_mon = Month.to_int (Date.month date) - 1
  ; tm_mday = Date.day date
  ; tm_hour = parts.hr
  ; tm_min = parts.min
  ; tm_sec = parts.sec
  ; tm_isdst =
      (* We don't keep track of "DST or not", so we use a dummy value. See caveat in
         interface about time zones and DST. *)
      false
  ; tm_wday = Day_of_week.to_int (Date.day_of_week date)
  ; tm_yday = Date.diff date (Date.create_exn ~y:(Date.year date) ~m:Jan ~d:1)
  }
;;

let format (t : t) s ~zone = Unix.strftime (to_tm t ~zone) s

let of_tm tm ~zone =
  (* Explicitly ignoring isdst, wday, yday (they are redundant with the other fields
     and the [zone] argument) *)
  let ({ tm_year
       ; tm_mon
       ; tm_mday
       ; tm_hour
       ; tm_min
       ; tm_sec
       ; tm_isdst = _
       ; tm_wday = _
       ; tm_yday = _
       }
        : Unix.tm)
    =
    tm
  in
  let date =
    Date.create_exn ~y:(tm_year + 1900) ~m:(Month.of_int_exn (tm_mon + 1)) ~d:tm_mday
  in
  let ofday = Ofday.create ~hr:tm_hour ~min:tm_min ~sec:tm_sec () in
  of_date_ofday ~zone date ofday
;;

let parse ?allow_trailing_input s ~fmt ~zone =
  Unix.strptime ?allow_trailing_input ~fmt s |> of_tm ~zone
;;

(* Does not represent extra hours due to DST (daylight saving time) (because DST makes
   adjustments in terms of wall clock time) or leap seconds (which aren't represented in
   Unix linear time).  See {!Ofday}. *)
module Ofday = struct
  include Time_ns.Ofday

  let arg_type = Core.Command.Arg_type.create of_string

  let of_ofday_float_round_nearest_microsecond core =
    of_span_since_start_of_day_exn
      (Span.of_span_float_round_nearest_microsecond
         (Time.Ofday.to_span_since_start_of_day core))
  ;;

  let of_ofday_float_round_nearest core =
    of_span_since_start_of_day_exn
      (Span.of_span_float_round_nearest (Time.Ofday.to_span_since_start_of_day core))
  ;;

  let of_time time ~zone = to_ofday time ~zone

  let to_ofday_float_round_nearest_microsecond t =
    Time.Ofday.of_span_since_start_of_day_exn
      (Span.to_span_float_round_nearest_microsecond (to_span_since_start_of_day t))
  ;;

  let to_ofday_float_round_nearest t =
    Time.Ofday.of_span_since_start_of_day_exn
      (Span.to_span_float_round_nearest (to_span_since_start_of_day t))
  ;;

  let now ~zone = of_time (Time_ns.now ()) ~zone

  (* Legacy conversions that round to the nearest microsecond *)
  let to_ofday = to_ofday_float_round_nearest_microsecond
  let of_ofday = of_ofday_float_round_nearest_microsecond

  (* This module is in [Time_ns_unix] instead of [Core] because to sexp a [Zone.t], we
     need to read a time zone database to work out DST transitions. We do not have a
     portable way to do that, and currently only support the operation on Unix. *)
  module Zoned = struct
    type t =
      { ofday : Time_ns.Ofday.t
      ; zone : Zone.t
      }
    [@@deriving bin_io, fields ~getters, compare, equal, hash]

    type sexp_repr = Time_ns.Ofday.t * Zone.t [@@deriving sexp]

    let sexp_of_t t = [%sexp_of: sexp_repr] (t.ofday, t.zone)

    let t_of_sexp sexp =
      let ofday, zone = [%of_sexp: sexp_repr] sexp in
      { ofday; zone }
    ;;

    let to_time_ns t date = of_date_ofday ~zone:(zone t) date (ofday t)
    let create ofday zone = { ofday; zone }
    let create_local ofday = create ofday (Lazy.force Zone.local)

    let of_string string : t =
      match String.split string ~on:' ' with
      | [ ofday; zone ] ->
        { ofday = Time_ns.Ofday.of_string ofday; zone = Zone.of_string zone }
      | _ -> failwithf "Ofday.Zoned.of_string %s" string ()
    ;;

    let to_string (t : t) : string =
      String.concat [ Time_ns.Ofday.to_string t.ofday; " "; Zone.to_string t.zone ]
    ;;

    let arg_type = Core.Command.Arg_type.create of_string

    module With_nonchronological_compare = struct
      type nonrec t = t [@@deriving bin_io, compare, equal, sexp, hash]
    end

    include Pretty_printer.Register (struct
      type nonrec t = t

      let to_string = to_string
      let module_name = "Time_ns_unix.Ofday.Zoned"
    end)

    module Stable = struct
      module V1 = struct
        let compare = With_nonchronological_compare.compare

        module Bin_repr = struct
          type nonrec t = t =
            { ofday : Time_ns.Stable.Ofday.V1.t
            ; zone : Timezone.Stable.V1.t
            }
          [@@deriving bin_io, stable_witness]
        end

        include
          Binable.Of_binable_without_uuid [@alert "-legacy"]
            (Bin_repr)
            (struct
              type nonrec t = t

              let to_binable t : Bin_repr.t = { ofday = ofday t; zone = zone t }
              let of_binable (repr : Bin_repr.t) = create repr.ofday repr.zone
            end)

        type nonrec t = t [@@deriving hash]

        let stable_witness : t Stable_witness.t = Bin_repr.stable_witness

        type sexp_repr = Time_ns.Stable.Ofday.V1.t * Timezone.Stable.V1.t
        [@@deriving sexp]

        let sexp_of_t t = [%sexp_of: sexp_repr] (ofday t, zone t)

        let t_of_sexp sexp =
          let ofday, zone = [%of_sexp: sexp_repr] sexp in
          create ofday zone
        ;;
      end
    end
  end

  module Option = struct
    type ofday = t [@@deriving sexp, compare]
    type t = Span.Option.t [@@deriving bin_io, compare, equal, hash, typerep]

    let none = Span.Option.none
    let some t = Span.Option.some (to_span_since_start_of_day t)
    let is_none = Span.Option.is_none
    let is_some = Span.Option.is_some

    let some_is_representable t =
      Span.Option.some_is_representable (to_span_since_start_of_day t)
    ;;

    let value t ~default =
      Bool.select
        (is_none t)
        default
        (of_span_since_start_of_day_unchecked (Span.Option.unchecked_value t))
    ;;

    let of_span_since_start_of_day span =
      if span_since_start_of_day_is_valid span then Span.Option.some span else none
    ;;

    let value_exn t =
      if is_some t
      then of_span_since_start_of_day_unchecked (Span.Option.unchecked_value t)
      else raise_s [%message [%here] "Time_ns_unix.Ofday.Option.value_exn none"]
    ;;

    let unchecked_value t =
      of_span_since_start_of_day_unchecked (Span.Option.unchecked_value t)
    ;;

    let of_option = function
      | None -> none
      | Some t -> some t
    ;;

    let to_option t = if is_none t then None else Some (value_exn t)

    (* Can't use the quickcheck generator and shrinker inherited from [Span.Option]
       because they may produce spans whose representation is larger than
       [start_of_next_day] *)
    let quickcheck_generator : t Quickcheck.Generator.t =
      Quickcheck.Generator.map
        ~f:of_option
        (Core.Option.quickcheck_generator
           (Quickcheck.Generator.filter
              ~f:some_is_representable
              Time_ns.Ofday.quickcheck_generator))
    ;;

    let quickcheck_shrinker : t Quickcheck.Shrinker.t =
      Quickcheck.Shrinker.map
        ~f:of_option
        ~f_inverse:to_option
        (Core.Option.quickcheck_shrinker
           (Base_quickcheck.Shrinker.filter
              ~f:some_is_representable
              Time_ns.Ofday.quickcheck_shrinker))
    ;;

    let quickcheck_observer = Span.Option.quickcheck_observer

    module Optional_syntax = struct
      module Optional_syntax = struct
        let is_none = is_none
        let unsafe_value = unchecked_value
      end
    end

    module Stable = struct
      module V1 = struct
        module T = struct
          type nonrec t = t [@@deriving compare, equal, bin_io]

          let stable_witness : t Stable_witness.t = Stable_witness.assert_stable
          let sexp_of_t t = [%sexp_of: Time_ns.Stable.Ofday.V1.t option] (to_option t)
          let t_of_sexp s = of_option ([%of_sexp: Time_ns.Stable.Ofday.V1.t option] s)
          let to_int63 t = Span.Option.Stable.V1.to_int63 t
          let of_int63_exn t = Span.Option.Stable.V1.of_int63_exn t
        end

        include T
        include Comparator.Stable.V1.Make (T)

        include Diffable.Atomic.Make (struct
          type nonrec t = t [@@deriving sexp, bin_io, equal]
        end)
      end
    end

    let sexp_of_t = Stable.V1.sexp_of_t
    let t_of_sexp = Stable.V1.t_of_sexp

    include Identifiable.Make (struct
      type nonrec t = t [@@deriving sexp, compare, bin_io, hash]

      let module_name = "Time_ns_unix.Ofday.Option"

      include Sexpable.To_stringable (struct
        type nonrec t = t [@@deriving sexp]
      end)
    end)

    include (Span.Option : Core.Comparisons.S with type t := t)

    include Diffable.Atomic.Make (struct
      type nonrec t = t [@@deriving sexp, bin_io, equal]
    end)
  end
end

let get_sexp_zone = Time.get_sexp_zone
let set_sexp_zone = Time.set_sexp_zone

let t_of_sexp_gen ~if_no_timezone sexp =
  try
    match sexp with
    | Sexp.List [ Sexp.Atom date; Sexp.Atom ofday; Sexp.Atom tz ] ->
      of_date_ofday ~zone:(Zone.find_exn tz) (Date.of_string date) (Ofday.of_string ofday)
    (* This is actually where the output of [sexp_of_t] is handled, since that's e.g.
       (2015-07-06 09:09:44.787988+01:00). *)
    | Sexp.List [ Sexp.Atom date; Sexp.Atom ofday_and_possibly_zone ] ->
      of_string_gen ~if_no_timezone (date ^ " " ^ ofday_and_possibly_zone)
    | Sexp.Atom datetime -> of_string_gen ~if_no_timezone datetime
    | _ -> of_sexp_error "Time.t_of_sexp" sexp
  with
  | Of_sexp_error _ as e -> raise e
  | e -> of_sexp_error (sprintf "Time.t_of_sexp: %s" (Exn.to_string e)) sexp
;;

let t_of_sexp sexp = t_of_sexp_gen sexp ~if_no_timezone:(`Use_this_one (get_sexp_zone ()))
let t_of_sexp_abs sexp = t_of_sexp_gen sexp ~if_no_timezone:`Fail

let t_sexp_grammar : t Sexplib.Sexp_grammar.t =
  { untyped =
      Union
        [ String
        ; List (Cons (String, Cons (String, Empty)))
        ; List (Cons (String, Cons (String, Cons (String, Empty))))
        ]
  }
;;

let sexp_of_t_abs t ~zone =
  Sexp.List (List.map (Time_ns.to_string_abs_parts ~zone t) ~f:(fun s -> Sexp.Atom s))
;;

let sexp_of_t t = sexp_of_t_abs ~zone:(get_sexp_zone ()) t
let of_date_ofday_zoned date ofday_zoned = Ofday.Zoned.to_time_ns ofday_zoned date

let to_date_ofday_zoned t ~zone =
  let date, ofday = to_date_ofday t ~zone in
  date, Ofday.Zoned.create ofday zone
;;

let to_ofday_zoned t ~zone =
  let ofday = to_ofday t ~zone in
  Ofday.Zoned.create ofday zone
;;

include Diffable.Atomic.Make (struct
  type nonrec t = t [@@deriving bin_io, equal, sexp]
end)

module Stable0 = struct
  module V1 = struct
    module T0 = struct
      (* We use the unstable serialization here, and rely on comprehensive tests of the
         stable conversion to make sure we don't change it. *)
      type nonrec t = t [@@deriving bin_io, compare, equal, hash, sexp]

      let stable_witness : t Stable_witness.t = Stable_witness.assert_stable
      let of_int63_exn t = of_span_since_epoch (Span.of_int63_ns t)
      let to_int63 t = to_int63_ns_since_epoch t
    end

    module T = struct
      include T0
      module Comparator = Comparator.Stable.V1.Make (T0)
      include Comparator
    end

    include T
    include Comparable.Stable.V1.With_stable_witness.Make (T)
    include Diffable.Atomic.Make (T)
  end
end

include Stable0.V1.Comparator

module Option = struct
  include Time_ns.Option

  module Stable = struct
    module V1 = struct
      module T = struct
        include Stable.V1

        let sexp_of_t t = [%sexp_of: Stable0.V1.t option] (to_option t)
        let t_of_sexp s = of_option ([%of_sexp: Stable0.V1.t option] s)
      end

      include T
      include Comparator.Stable.V1.Make (T)

      include Diffable.Atomic.Make (struct
        include T

        let equal = [%compare.equal: t]
      end)
    end
  end

  let sexp_of_t = Stable.V1.sexp_of_t
  let t_of_sexp = Stable.V1.t_of_sexp

  include Identifiable.Make (struct
    type nonrec t = t [@@deriving sexp, compare, bin_io, hash]

    let module_name = "Time_ns_unix.Option"

    include Sexpable.To_stringable (struct
      type nonrec t = t [@@deriving sexp]
    end)
  end)

  (* bring back the efficient implementation of comparison operators *)
  include (Time_ns.Option : Core.Comparisons.S with type t := t)

  include Diffable.Atomic.Make (struct
    type nonrec t = t [@@deriving bin_io, equal, sexp]
  end)
end

(* Note: This is FIX standard millisecond precision. You should use
   [Zero.Time_ns_with_fast_accurate_to_of_string] if you need nanosecond precision. *)
let to_string_fix_proto zone t =
  Time.to_string_fix_proto zone (to_time_float_round_nearest_microsecond t)
;;

let of_string_fix_proto zone s =
  of_time_float_round_nearest_microsecond (Time.of_string_fix_proto zone s)
;;

include Identifiable.Make_using_comparator (struct
  include Stable0.V1

  let module_name = "Time_ns_unix"
  let of_string, to_string = of_string, to_string
end)

(* bring back the efficient implementation of comparison operators *)
include (Core.Time_ns : Core.Comparisons.S with type t := t)

module Stable = struct
  module Option = Option.Stable

  module Span = struct
    include Span.Stable
    module Option = Span.Option.Stable
  end

  module Ofday = struct
    include Time_ns.Stable.Ofday
    module Zoned = Ofday.Zoned.Stable
    module Option = Ofday.Option.Stable
  end

  module Zone = Timezone.Stable
  include Stable0
  module Alternate_sexp = Core.Time_ns.Stable.Alternate_sexp
end

(*
   Dropping Time in favor of Time_ns is possible and has been discussed, but we have
   chosen not to do so at this time for a few reasons:

   - It's a lot of work.  All functions over Time, including the related
     modules Date, Ofday, Zone, Span, Schedule have to be converted to Time_ns
     space.  This is largely mechanical, but will create a lot of churn within
     the modules and possibly externally where the floatiness of the Time world
     leaks out.

   - It's of limited utility compared to other things we could be working on.
     Time math would be easier to understand and somewhat faster, but very few
     modules/programs would benefit from faster time math.  Those that do can
     use Time_ns already for the most part.

   - Having Time_ns and a conversion function already gives the bulk of the
     value to programs that want a fast, non-allocating version of [Time.now].
     Indeed, many remaining unconverted functions

   - We aren't certain about how the boundaries around Time_ns will affect the
     external viability of Core.  Internally we don't think being limited to
     a smaller time range is an issue, and really far off times are better
     represented as (Date.t * Ofday.t), but it is still a restriction.  This
     pushback is probably minimal and, if we could get over the work concerns,
     could be eliminated.

   - Converting between Time and Time_ns when you use libraries based on different ones
     isn't so bad. (?)
*)
