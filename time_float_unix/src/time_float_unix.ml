open! Core
open! Import
include Time_float

let of_tm tm ~zone =
  (* Explicitly ignoring isdst, wday, yday (they are redundant with the other fields
       and the [zone] argument) *)
  let { Unix.tm_year
      ; tm_mon
      ; tm_mday
      ; tm_hour
      ; tm_min
      ; tm_sec
      ; tm_isdst = _
      ; tm_wday = _
      ; tm_yday = _
      }
    =
    tm
  in
  let date =
    Date.create_exn ~y:(tm_year + 1900) ~m:(Month.of_int_exn (tm_mon + 1)) ~d:tm_mday
  in
  let ofday = Ofday.create ~hr:tm_hour ~min:tm_min ~sec:tm_sec () in
  of_date_ofday ~zone date ofday
;;

let format t s ~zone =
  let epoch_time =
    Zone.date_and_ofday_of_absolute_time zone t
    |> Date_and_ofday.to_synthetic_span_since_epoch
    |> Span.to_sec
  in
  Unix.strftime (Unix.gmtime epoch_time) s
;;

let parse ?allow_trailing_input s ~fmt ~zone =
  Unix.strptime ?allow_trailing_input ~fmt s |> of_tm ~zone
;;

let pause_for span =
  let time_remaining =
    (* If too large a float is passed in (Span.max_value for instance) then
         nanosleep will return immediately, leading to an infinite and expensive
         select loop.  This is handled by pausing for no longer than 100 days.
    *)
    let span = Span.min span (Span.scale Span.day 100.) in
    Unix.nanosleep (Span.to_sec span)
  in
  if Float.( > ) time_remaining 0.0 then `Remaining (Span.of_sec time_remaining) else `Ok
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
  pause (Span.of_day 1.0);
  pause_forever ()
;;
