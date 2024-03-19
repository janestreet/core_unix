open Core
open Expect_test_helpers_core
module Unix = Core_unix
module Time = Time_float_unix
module Date = Date
module Ofday = Time.Ofday
module Span = Time.Span

(* Used in all but two tests that explicitly specify UTC *)
let zone = force Time.Zone.local

let require_span_equal_ms_precision here s1 s2 =
  require_equal
    here
    (module Int63)
    (Float.int63_round_nearest_exn (Time.Span.to_ms s1))
    (Float.int63_round_nearest_exn (Time.Span.to_ms s2))
;;

let require_time_equal_ms_precision here t1 t2 =
  require_span_equal_ms_precision
    here
    (Time.to_span_since_epoch t1)
    (Time.to_span_since_epoch t2)
;;

let require_similar_time here time time' =
  let time = Time.to_span_since_epoch time |> Time.Span.to_sec in
  let time' = Time.to_span_since_epoch time' |> Time.Span.to_sec in
  let delta = Float.abs (time -. time') in
  require
    here
    (Float.( < ) delta 0.01)
    ~if_false_then_print_s:(lazy [%message (delta : float)])
;;

let%expect_test "t" =
  let s1 = "2005-05-25 12:46-4:00" in
  let s2 = "2005-05-25 12:46:15-4:00" in
  let s3 = "2005-05-25 12:46:15.232-4:00" in
  let s4 = "2005-05-25 12:46:15.232338-4:00" in
  let time1 = Time.of_string s1 in
  let time2 = Time.of_string s2 in
  let time3 = Time.of_string s3 in
  let time4 = Time.of_string s4 in
  let now1 = Time.now () in
  let now2 = Time.now () in
  print_s
    [%sexp
      (Float.iround_exn ~dir:`Nearest (Time.Span.to_sec (Time.diff time2 time1)) : int)];
  [%expect {| 15 |}];
  print_s
    [%sexp
      (Float.iround_exn ~dir:`Nearest (Time.Span.to_ms (Time.diff time2 time1)) : int)];
  [%expect {| 15000 |}];
  print_s
    [%sexp
      (Float.iround_exn ~dir:`Nearest (Time.Span.to_ms (Time.diff time3 time2)) : int)];
  [%expect {| 232 |}];
  print_s
    [%sexp
      (Float.iround_exn ~dir:`Nearest (Time.Span.to_us (Time.diff time4 time3)) : int)];
  [%expect {| 338 |}];
  require [%here] (Time.( >= ) now2 now1);
  let test_sexp_roundtrip time =
    require_equal [%here] (module Time) (Time.t_of_sexp (Time.sexp_of_t time)) time
  in
  test_sexp_roundtrip time1;
  test_sexp_roundtrip time2;
  test_sexp_roundtrip time3;
  let date, ofday = Time.to_date_ofday time3 ~zone in
  print_s [%message (date : Date.t) (ofday : Time.Ofday.t)];
  [%expect {|
    ((date  2005-05-25)
     (ofday 12:46:15.232000))
    |}];
  require_equal
    [%here]
    (module Time.Ofday)
    (Time.Ofday.of_string "09:13")
    (Time.Ofday.of_string "0913");
  require_time_equal_ms_precision [%here] (Time.add time1 (sec 15.)) time2;
  require_time_equal_ms_precision [%here] (Time.add time2 (Time.Span.of_ms 232.)) time3
;;

let%expect_test "Ofday_string_conversion" =
  (* We want to test a number of times during the day, but testing all
     possible times is too expensive.  We also want the test to always be
     the same, so this uses a specific Random.State to generate a repeatable
     series of random times to test *)
  let rand_state = Random.State.make [| 1; 2; 3; 4; 5; 6; 7 |] in
  for _ = 0 to 100_000 do
    let secs = Random.State.int rand_state 86_400_000 in
    let ofday = Ofday.of_span_since_start_of_day_exn (Time.Span.of_ms (float secs)) in
    let ofday_string = Ofday.to_string ofday in
    let ofday' = Ofday.of_string ofday_string in
    if Ofday.( <>. ) ofday ofday'
    then
      failwithf
        "(%d seconds) %s (%.20f) <> Ofday.of_string %s (%.20f)"
        secs
        ofday_string
        (Ofday.to_span_since_start_of_day ofday |> Time.Span.to_sec)
        (Ofday.to_string ofday')
        (Ofday.to_span_since_start_of_day ofday' |> Time.Span.to_sec)
        ();
    let ofday' = Ofday.of_string ofday_string in
    if Ofday.( <>. ) ofday ofday'
    then failwithf "%s <> Ofday.of_string %s" ofday_string (Ofday.to_string ofday') ()
  done
;;

let%expect_test "date" =
  let start =
    Time.of_date_ofday
      ~zone
      (Date.create_exn ~y:1999 ~m:Month.Jan ~d:1)
      Ofday.start_of_day
  in
  let day = Span.of_day 1. in
  let number_of_days =
    match Word_size.word_size with
    | W64 -> 100_000
    | W32 -> 365 * 39
    (* do not go pass 2038 *)
  in
  for i = 0 to number_of_days do
    let date = Time.to_date ~zone (Time.add start (Time.Span.scale day (float i))) in
    let date_string = Date.to_string date in
    let date' = Date.of_string date_string in
    if Date.( <> ) date date'
    then failwithf "%s <> Date.of_string %s" date_string (Date.to_string date') ()
  done
;;

let%test_module "compare with old implementation" =
  (module struct
    let of_tm tm =
      Date.create_exn
        ~y:(tm.Unix.tm_year + 1900)
        ~m:(Month.of_int_exn (tm.Unix.tm_mon + 1))
        ~d:tm.Unix.tm_mday
    ;;

    let to_tm t =
      { Unix.tm_sec = 0
      ; tm_min = 0
      ; tm_hour = 12
      ; tm_mday = Date.day t
      ; tm_mon = Month.to_int (Date.month t) - 1
      ; tm_year = Date.year t - 1900
      ; tm_wday = 0
      ; tm_yday = 0
      ; tm_isdst = false
      }
    ;;

    let to_time_internal t =
      let tm_date = to_tm t in
      let time = fst (Unix.mktime tm_date) in
      Time.of_span_since_epoch (Time.Span.of_sec time)
    ;;

    let of_time_internal time =
      of_tm
        (Unix.localtime
           (Float.round ~dir:`Down (Time.to_span_since_epoch time |> Time.Span.to_sec)))
    ;;

    let add_days t n =
      let time = to_time_internal t in
      of_time_internal (Time.add time (Span.of_day (Float.of_int n)))
    ;;

    let day_of_week t =
      let uday = to_tm t in
      let sec, _ = Unix.mktime uday in
      let unix_wday = (Unix.localtime sec).Unix.tm_wday in
      Day_of_week.of_int_exn unix_wday
    ;;

    let exhaustive_date_range =
      if Sys_unix.c_int_size () < 64
      then Date.create_exn ~y:1970 ~m:Month.Jan ~d:1, 365 * 68
      else Date.create_exn ~y:1900 ~m:Month.Jan ~d:1, 365 * 100
    ;;

    let%expect_test "exhaustive day_of_week test" =
      let start_date, ndays = exhaustive_date_range in
      let rec loop n current_date =
        if n = ndays
        then ()
        else (
          let old_method = day_of_week current_date in
          let new_method = Date.day_of_week current_date in
          if Day_of_week.( = ) new_method old_method
          then loop (n + 1) (Date.add_days current_date 1)
          else
            print_cr
              [%here]
              [%message
                "Implementations do not match"
                  (n : int)
                  (current_date : Date.t)
                  (old_method : Day_of_week.t)
                  (new_method : Day_of_week.t)])
      in
      loop 1 start_date
    ;;

    let%expect_test "exhaustive add_days test" =
      let start_date, ndays = exhaustive_date_range in
      let rec loop n current_date =
        if n = ndays
        then ()
        else (
          let old_method = add_days current_date 1 in
          let new_method = Date.add_days current_date 1 in
          if Date.( = ) old_method new_method
          then loop (n + 1) new_method
          else
            print_cr
              [%here]
              [%message
                "Implementations do not match"
                  (n : int)
                  (current_date : Date.t)
                  (old_method : Date.t)
                  (new_method : Date.t)])
      in
      loop 1 start_date
    ;;
  end)
;;

let%expect_test "add_days" =
  print_s [%sexp (Date.add_days (Date.of_string "2008-11-02") 1 : Date.t)];
  [%expect {| 2008-11-03 |}];
  print_s [%sexp (Date.add_days (Date.of_string "2008-11-02") 2 : Date.t)];
  [%expect {| 2008-11-04 |}];
  print_s [%sexp (Date.add_days (Date.of_string "2000-02-28") 1 : Date.t)];
  [%expect {| 2000-02-29 |}]
;;

let%expect_test "add_months" =
  print_s [%sexp (Date.add_months (Date.of_string "2009-02-28") 0 : Date.t)];
  [%expect {| 2009-02-28 |}];
  print_s [%sexp (Date.add_months (Date.of_string "2009-01-30") 1 : Date.t)];
  [%expect {| 2009-02-28 |}];
  print_s [%sexp (Date.add_months (Date.of_string "2009-01-30") 2 : Date.t)];
  [%expect {| 2009-03-30 |}];
  print_s [%sexp (Date.add_months (Date.of_string "2009-02-28") 10 : Date.t)];
  [%expect {| 2009-12-28 |}];
  print_s [%sexp (Date.add_months (Date.of_string "2009-01-30") (-11) : Date.t)];
  [%expect {| 2008-02-29 |}]
;;

let%expect_test "add_weekdays_rounding_forward" =
  let test d1 n =
    print_s [%sexp (Date.add_weekdays_rounding_forward (Date.of_string d1) n : Date.t)]
  in
  test "2009-01-01" 1;
  [%expect {| 2009-01-02 |}];
  test "2009-01-02" 1;
  [%expect {| 2009-01-05 |}];
  test "2009-01-02" (-1);
  [%expect {| 2009-01-01 |}];
  test "2009-01-05" (-1);
  [%expect {| 2009-01-02 |}];
  test "2009-01-06" (-2);
  [%expect {| 2009-01-02 |}];
  test "2009-02-27" 1;
  [%expect {| 2009-03-02 |}];
  test "2008-02-28" 2;
  [%expect {| 2008-03-03 |}]
;;

let%expect_test "add_weekdays_rounding_backward" =
  let test d1 n =
    print_s [%sexp (Date.add_weekdays_rounding_backward (Date.of_string d1) n : Date.t)]
  in
  test "2009-01-01" 1;
  [%expect {| 2009-01-02 |}];
  test "2009-01-02" 1;
  [%expect {| 2009-01-05 |}];
  test "2009-01-02" (-1);
  [%expect {| 2009-01-01 |}];
  test "2009-01-05" (-1);
  [%expect {| 2009-01-02 |}];
  test "2009-01-06" (-2);
  [%expect {| 2009-01-02 |}];
  test "2009-02-27" 1;
  [%expect {| 2009-03-02 |}];
  test "2008-02-28" 2;
  [%expect {| 2008-03-03 |}]
;;

let%expect_test "add_business_days_rounding_forward" =
  let test d1 n =
    let is_holiday d =
      List.mem
        ~equal:Date.equal
        (List.map [ "2009-01-01"; "2009-03-01"; "2009-03-02" ] ~f:Date.of_string)
        d
    in
    print_s
      [%sexp
        (Date.add_business_days_rounding_forward ~is_holiday (Date.of_string d1) n
          : Date.t)]
  in
  test "2009-01-01" 1;
  [%expect {| 2009-01-05 |}];
  test "2009-01-02" 1;
  [%expect {| 2009-01-05 |}];
  test "2009-01-02" (-1);
  [%expect {| 2008-12-31 |}];
  test "2009-01-05" (-1);
  [%expect {| 2009-01-02 |}];
  test "2009-01-06" (-2);
  [%expect {| 2009-01-02 |}];
  test "2009-02-27" 1;
  [%expect {| 2009-03-03 |}];
  test "2008-02-28" 2;
  [%expect {| 2008-03-03 |}]
;;

let%expect_test "add_business_days_rounding_backward" =
  let test d1 n =
    let is_holiday d =
      List.mem
        ~equal:Date.equal
        (List.map [ "2009-01-01"; "2009-03-01"; "2009-03-02" ] ~f:Date.of_string)
        d
    in
    print_s
      [%sexp
        (Date.add_business_days_rounding_backward ~is_holiday (Date.of_string d1) n
          : Date.t)]
  in
  test "2009-01-01" 1;
  [%expect {| 2009-01-02 |}];
  test "2009-01-02" 1;
  [%expect {| 2009-01-05 |}];
  test "2009-01-02" (-1);
  [%expect {| 2008-12-31 |}];
  test "2009-01-05" (-1);
  [%expect {| 2009-01-02 |}];
  test "2009-01-06" (-2);
  [%expect {| 2009-01-02 |}];
  test "2009-02-27" 1;
  [%expect {| 2009-03-03 |}];
  test "2008-02-28" 2;
  [%expect {| 2008-03-03 |}]
;;

let%expect_test "span_scale" =
  require_span_equal_ms_precision
    [%here]
    (Time.Span.scale (sec 10.) 0.001)
    (Time.Span.of_ms 10.);
  require_span_equal_ms_precision
    [%here]
    (Time.Span.scale (sec 10.) 60.)
    (Time.Span.of_min 10.);
  require_span_equal_ms_precision
    [%here]
    (Time.Span.scale (sec 10.) (60. *. 60.))
    (Time.Span.of_hr 10.)
;;

let%expect_test "span_conv" =
  let test_span_conv ?tolerance of_span to_span x =
    let tolerance =
      match tolerance with
      | None -> Float.robust_comparison_tolerance
      | Some pct -> x *. pct
    in
    let delta = Float.abs (of_span (to_span x) -. x) in
    require
      [%here]
      (Float.( <= ) delta tolerance)
      ~if_false_then_print_s:(lazy [%message (delta : float)])
  in
  let test ?tolerance here of_span to_span =
    quickcheck_m
      here
      (module struct
        type t = float [@@deriving quickcheck, sexp_of]

        let quickcheck_generator = Float.gen_incl 0.001 1_000.
      end)
      ~f:(test_span_conv ?tolerance of_span to_span)
  in
  test [%here] Time.Span.to_sec sec;
  test [%here] Time.Span.to_ms Span.of_ms;
  test [%here] (fun x -> Time.Span.to_sec x /. 60.) Span.of_min;
  test [%here] (fun x -> Time.Span.to_sec x /. 60. /. 60.) Span.of_hr;
  test
    [%here]
    ~tolerance:0.0001
    (fun x -> Time.Span.to_sec (Time.Span.t_of_sexp x))
    (fun x -> Span.sexp_of_t (sec x))
;;

let%expect_test " date" =
  let d = Date.create_exn ~y:2004 ~m:Month.Apr ~d:15 in
  print_endline (Date.to_string d);
  [%expect {| 2004-04-15 |}];
  let test_alternative_string string =
    require_equal [%here] (module Date) (Date.of_string string) d
  in
  test_alternative_string "2004-04-15";
  test_alternative_string "20040415";
  test_alternative_string "15APR2004";
  test_alternative_string "04/15/2004";
  test_alternative_string "2004/04/15";
  test_alternative_string "4/15/4"
;;

let%expect_test "norollover" =
  let t1 = Time.of_localized_string ~zone "2005-05-25 12:46:59.900" in
  let t2 = Time.add t1 (Time.Span.of_ms 99.9) in
  (* within 1 mic *)
  print_endline (Time.to_string_abs ~zone t2);
  [%expect {| 2005-05-25 12:46:59.999900-04:00 |}]
;;

let%expect_test "quickcheck tests of string roundtrip" =
  quickcheck_m
    [%here]
    (module Time)
    ~f:(fun time ->
      require_similar_time [%here] time (Time.of_string (Time.to_string time));
      require_similar_time
        [%here]
        time
        (Time.of_filename_string ~zone (Time.to_filename_string time ~zone));
      require_similar_time [%here] time (Time.t_of_sexp (Time.sexp_of_t time)))
;;

let%expect_test "to_string,of_string2" =
  let test_to_string_abs_roundtrip s =
    let t = Time.of_string s in
    require_equal [%here] (module String) (Time.to_string_abs t ~zone) s
  in
  test_to_string_abs_roundtrip "2005-06-01 10:15:08.047123-04:00";
  test_to_string_abs_roundtrip "2006-06-16 04:37:07.082945-04:00"
;;

let%expect_test "of_string without colon, negative offset" =
  require_equal
    [%here]
    (module Time)
    (Time.of_string_abs "2015-07-14 10:31:55.564871-04:00")
    (Time.of_string_abs "2015-07-14 10:31:55.564871-0400")
;;

let%expect_test "of_string without colon, positive offset" =
  require_equal
    [%here]
    (module Time)
    (Time.of_string_abs "2015-07-14 10:31:55.564871+04:00")
    (Time.of_string_abs "2015-07-14 10:31:55.564871+0400")
;;

let%expect_test "of_string with leap second" =
  let expected_time_at_leap_second =
    Time.of_date_ofday
      ~zone:Time.Zone.utc
      (Date.create_exn ~y:2015 ~m:Jul ~d:1)
      Time.Ofday.start_of_day
  in
  List.iter [ "2015-06-30 23:59:60Z"; "2015-06-30 23:59:60.500Z" ] ~f:(fun s ->
    require_equal [%here] (module Time) (Time.of_string s) expected_time_at_leap_second)
;;

let%expect_test "to_filename_string,of_filename_string2" =
  let test s =
    let t = Time.of_filename_string s ~zone in
    let s_roundtrip = Time.to_filename_string t ~zone in
    require_equal [%here] (module String) s s_roundtrip;
    print_s [%sexp (t : Time.t)]
  in
  test "2005-06-01_10-15-08.047983";
  [%expect {| (2005-06-01 10:15:08.047983-04:00) |}]
;;

let%expect_test "daylight_saving_time" =
  let s = "2006-04-02 23:00:00.000000-04:00" in
  let time = Time.of_string s in
  let s_roundtrip = Time.to_string_abs ~zone time in
  require_equal [%here] (module String) s s_roundtrip
;;

let%expect_test "weird_date_in_time" =
  let test s =
    let time = Time.of_string s in
    print_endline (Time.to_string_abs time ~zone)
  in
  test "01 JAN 2008 10:37:22.551-05:00";
  [%expect {| 2008-01-01 10:37:22.551000-05:00 |}];
  test "01 FEB 2008 17:38:44.031-05:00";
  [%expect {| 2008-02-01 17:38:44.031000-05:00 |}]
;;

let%expect_test "ofday_small_diff" =
  let require_same here x y =
    require here (Float.( < ) (Float.abs (x -. y)) (sqrt Float.epsilon_float))
  in
  let check (s1, s2, d) =
    let t1 = Time.Ofday.of_string s1 in
    let t2 = Time.Ofday.of_string s2 in
    require_same [%here] (Time.Span.to_sec (Time.Ofday.small_diff t1 t2)) d;
    require_same [%here] (Time.Span.to_sec (Time.Ofday.small_diff t2 t1)) ~-.d
  in
  List.iter
    ~f:check
    [ "10:00:01.298", "14:59:55.000", 6.298
    ; "08:59:54.000", "10:00:01.555", -7.555
    ; "12:48:55.787", "17:48:55.000", 0.787
    ]
;;

let%expect_test "occurrence_right_side" =
  let times =
    [ "00:00:00"
    ; "00:00:01"
    ; "09:00:00"
    ; "11:59:59"
    ; "12:00:00"
    ; "12:00:01"
    ; "18:30:30"
    ; "23:59:59"
    ]
  in
  let now = Time.now () in
  let now_f = Time.to_span_since_epoch now |> Time.Span.to_sec in
  let utimes = Time.to_ofday ~zone now :: List.map times ~f:Time.Ofday.of_string in
  let after_times =
    List.map utimes ~f:(fun ut -> Time.occurrence `First_after_or_at now ~zone ~ofday:ut)
  in
  let before_times =
    List.map utimes ~f:(fun ut -> Time.occurrence `Last_before_or_at now ~zone ~ofday:ut)
  in
  List.iter after_times ~f:(fun t ->
    require [%here] (Float.( >= ) (Time.Span.to_sec (Time.to_span_since_epoch t)) now_f));
  List.iter before_times ~f:(fun t ->
    require [%here] (Float.( <= ) (Time.Span.to_sec (Time.to_span_since_epoch t)) now_f))
;;

let%expect_test "occurrence_distance" =
  let now = Time.of_string "2007-05-04 13:00:00.000" in
  let after_times =
    [ "13:00:00.000", "2007-05-04 13:00:00.000"
    ; "13:00:00.001", "2007-05-04 13:00:00.001"
    ; "11:59:59.999", "2007-05-05 11:59:59.999"
    ; "00:00:00.000", "2007-05-05 00:00:00.000"
    ; "12:59:59.000", "2007-05-05 12:59:59.000"
    ]
  in
  let before_times =
    [ "13:00:00.000", "2007-05-04 13:00:00.000"
    ; "13:00:00.001", "2007-05-03 13:00:00.001"
    ; "11:59:59.999", "2007-05-04 11:59:59.999"
    ; "00:00:00.000", "2007-05-04 00:00:00.000"
    ; "12:59:59.000", "2007-05-04 12:59:59.000"
    ]
  in
  List.iter after_times ~f:(fun (od_s, prediction_s) ->
    let od = Time.Ofday.of_string od_s in
    let prediction = Time.of_string prediction_s in
    let real = Time.occurrence `First_after_or_at now ~zone ~ofday:od in
    require_equal [%here] (module Float) (Time.Span.to_ms (Time.diff prediction real)) 0.);
  List.iter before_times ~f:(fun (od_s, prediction_s) ->
    let od = Time.Ofday.of_string od_s in
    let prediction = Time.of_string prediction_s in
    let real = Time.occurrence `Last_before_or_at now ~zone ~ofday:od in
    require_equal [%here] (module Float) (Time.Span.to_ms (Time.diff prediction real)) 0.)
;;

let%expect_test "diff" =
  let d1 = Date.create_exn ~y:2000 ~m:Month.Jan ~d:1 in
  let d2 = Date.create_exn ~y:2000 ~m:Month.Jan ~d:2 in
  let d3 = Date.create_exn ~y:2000 ~m:Month.Feb ~d:28 in
  let d4 = Date.create_exn ~y:2000 ~m:Month.Mar ~d:1 in
  print_s [%sexp (Date.diff d2 d1 : int)];
  [%expect {| 1 |}];
  print_s [%sexp (Date.diff d4 d3 : int)];
  [%expect {| 2 |}]
;;

let roundtrip s =
  let t = Span.of_string s in
  (* we only test rountrip in one direction because the other direction does not hold!
     for example 1.34m comes back as 1m20.400000000000006s *)
  require_equal [%here] (module Span) t (Time.Span.of_string (Time.Span.to_string t))
;;

let%expect_test "roundtrip span<->string" =
  let extensions = [ "ms"; "s"; "m"; "h" ] in
  List.iter extensions ~f:(fun ext ->
    let t x = roundtrip (x ^ ext) in
    t "1";
    t "5";
    t "1.34");
  let t x = roundtrip (x ^ "s") in
  t "59.9999";
  t "59"
;;

let%expect_test "Span.of_string (nan)" =
  require_does_raise [%here] (fun () -> Time.Span.of_string "nans");
  [%expect {| ("Time.Span.of_string: invalid span part magnitude" nans) |}]
;;

let%expect_test "Span.of_string (inf)" =
  require_does_raise [%here] (fun () -> Time.Span.of_string "infs");
  [%expect {| ("Time.Span.of_string: invalid span part magnitude" infs) |}]
;;

let%expect_test "Span.of_string" =
  let test string secs =
    require_equal
      [%here]
      (module Float)
      (Time.Span.to_sec (Time.Span.of_string string))
      secs
  in
  test "1ms" 0.001;
  test "95ms" 0.095;
  test "1222ms" 1.222;
  test "1.222s" 1.222;
  test "0.5m" 30.;
  test "1m" 60.;
  test "1h" (60. *. 60.)
;;

let%expect_test "Time.of_string_fix_proto" =
  require_equal
    [%here]
    (module Time)
    (Time.of_string_fix_proto `Utc "20080603-13:55:35.577")
    (Time.of_span_since_epoch
       (Time.Span.of_sec (Int64.float_of_bits 4742872407195577745L)))
;;
