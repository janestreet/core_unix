open! Core
module Unix = Core_unix
include Time_ns

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

let format ?locale (t : t) s ~zone = Unix.strftime ?locale (to_tm t ~zone) s

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

let parse ?locale ?allow_trailing_input s ~fmt ~zone =
  Unix.strptime ?locale ?allow_trailing_input ~fmt s |> of_tm ~zone
;;
