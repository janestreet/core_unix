open Core
open Expect_test_helpers_core
module Unix = Core_unix
module Time = Time_float_unix

(* We don't test Feb 29th because generating proper leap year dates is
   trickier.  Also, there are no time zone changes on leap dates. *)
let month_limits =
  Int.Map.of_alist_exn
    [ 1, 31
    ; 2, 28
    ; 3, 31
    ; 4, 30
    ; 5, 31
    ; 6, 30
    ; 7, 31
    ; 8, 31
    ; 9, 30
    ; 10, 31
    ; 11, 30
    ; 12, 31
    ]
;;

module Time_parts = struct
  type t =
    { year : int
    ; month : int
    ; day : int
    ; hour : int
    ; min : int
    ; sec : int
    ; ms : int
    ; us : int
    }

  let quickcheck_generator =
    let open Quickcheck.Generator.Let_syntax in
    let%bind year = Int.gen_incl 1970 2036
    and month = Int.gen_incl 1 12 in
    let%map day = Int.gen_incl 1 (Map.find_exn month_limits month)
    and hour = Int.gen_incl 8 19
    and min = Int.gen_incl 0 59
    and sec = Int.gen_incl 0 59
    and ms = Int.gen_incl 0 999
    and us = Int.gen_incl 0 999 in
    { year; month; day; hour; min; sec; ms; us }
  ;;
end

module Time_string : sig
  type t = private string [@@deriving quickcheck, sexp_of]
end = struct
  type t = string [@@deriving sexp_of]

  let quickcheck_generator =
    let%map.Quickcheck.Generator { year; month; day; hour; min; sec; ms; us = _ } =
      Time_parts.quickcheck_generator
    in
    sprintf "%d-%.2d-%.2d %.2d:%.2d:%.2d.%.3d000" year month day hour min sec ms
  ;;

  let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
  let quickcheck_observer = Quickcheck.Observer.of_hash (module String)
end

module Tm = struct
  type t = Unix.tm =
    { tm_sec : int
    ; tm_min : int
    ; tm_hour : int
    ; tm_mday : int
    ; tm_mon : int
    ; tm_year : int
    ; tm_wday : int
    ; tm_yday : int
    ; tm_isdst : bool
    }
  [@@deriving equal, sexp_of]

  let quickcheck_generator =
    let%map.Quickcheck.Generator { year; month; day; hour; min; sec; ms = _; us = _ } =
      Time_parts.quickcheck_generator
    in
    { Unix.tm_sec = sec
    ; tm_min = min
    ; tm_hour = hour
    ; tm_mday = day
    ; tm_mon = month
    ; tm_year = year - 1900
    ; tm_wday = 0
    ; tm_yday = 0
    ; tm_isdst = false
    }
  ;;

  let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
end

let%expect_test "quickcheck string roundtrip" =
  quickcheck_m
    [%here]
    (module struct
      type t =
        Time_string.t
        * (string[@quickcheck.generator Quickcheck.Generator.of_list [ "+"; "-" ]])
        * (int[@quickcheck.generator Int.gen_incl 1 10])
      [@@deriving quickcheck, sexp_of]
    end)
    ~f:(fun (base_string, pos_neg, distance) ->
      let base_string = (base_string :> string) in
      let s_with_random_offset = [%string "%{base_string}%{pos_neg}%{distance#Int}:00"] in
      let zone = force Time.Zone.local in
      let s_with_local_offset =
        let t = Time.of_string base_string in
        let same_date_ofday_in_utc =
          Time.Zone.date_and_ofday_of_absolute_time zone t
          |> Time.Zone.absolute_time_of_date_and_ofday Time.Zone.utc
        in
        let offset_sec =
          Time.diff same_date_ofday_in_utc t |> Time.Span.to_sec |> Float.to_int
        in
        let suffix =
          if offset_sec = 0 then "Z" else Printf.sprintf "%+03d:00" (offset_sec / 3600)
        in
        base_string ^ suffix
      in
      require_equal
        [%here]
        (module String)
        s_with_local_offset
        (Time.to_string_abs (Time.of_string s_with_local_offset) ~zone);
      let s2_time1 = Time.of_string s_with_random_offset in
      let s2_time2 = Time.of_string (Time.to_string_abs s2_time1 ~zone) in
      require [%here] (Time.( =. ) s2_time1 s2_time2))
;;

let test_roundtrip_conversion (zone_name, zone) =
  quickcheck_m
    [%here]
    (module Time)
    ~f:(fun time ->
      let time =
        (* Round to some whole number of seconds *)
        time
        |> Time.to_span_since_epoch
        |> Time.Span.to_int63_seconds_round_down_exn
        |> Time.Span.of_int63_seconds
        |> Time.of_span_since_epoch
      in
      let zone_date, zone_ofday =
        let date, ofday = Time.to_date_ofday ~zone:(force Time.Zone.local) time in
        Time.convert ~from_tz:(force Time.Zone.local) ~to_tz:zone date ofday
      in
      let round_trip_time =
        let round_date, round_ofday =
          Time.convert ~from_tz:zone ~to_tz:(force Time.Zone.local) zone_date zone_ofday
        in
        Time.of_date_ofday ~zone:(force Time.Zone.local) round_date round_ofday
      in
      require_equal
        [%here]
        (module Time)
        time
        round_trip_time
        ~if_false_then_print_s:(lazy [%message (zone_name : string) (zone : Time.Zone.t)]))
;;

let load_some_other_time_zones =
  lazy
    ([ "chi"; "nyc"; "hkg"; "lon"; "tyo" ]
     |> List.iter ~f:(fun name -> ignore (Time.Zone.find name : Time.Zone.t option)))
;;

let%expect_test "roundtrip conversion" =
  force load_some_other_time_zones;
  List.iter (Time.Zone.initialized_zones ()) ~f:(fun (name, zone) ->
    print_endline name;
    test_roundtrip_conversion (name, zone));
  [%expect
    {|
    America/Chicago
    America/New_York
    Asia/Hong_Kong
    Asia/Tokyo
    Europe/London
    |}]
;;

let%expect_test "random test against Unix.localtime" =
  force load_some_other_time_zones;
  quickcheck_m
    ~config:{ Base_quickcheck.Test.default_config with test_count = 100 }
    [%here]
    (module Tm)
    ~f:(fun tm ->
      (* Fill in [wday], [yday] fields *)
      let tm = Unix.gmtime (Unix.timegm tm) in
      List.iter (Time.Zone.initialized_zones ()) ~f:(fun (zone_name, zone) ->
        (* goes through the dance of setting the env variable, then calling localtime, then
           setting the TZ back.  We call localtime on 1000. each time to reset the internal
           state of localtime, which matters when we convert indeterminate times. *)
        Unix.putenv ~key:"TZ" ~data:zone_name;
        ignore (Unix.localtime 1000. : Unix.tm);
        let unix_time, _ = Unix.mktime tm in
        let localtime = Unix.localtime unix_time in
        let localtime_date_string = Unix.strftime localtime "%Y-%m-%d" in
        let localtime_ofday_string = Unix.strftime localtime "%H:%M:%S.000000" in
        Unix.unsetenv "TZ";
        ignore (Unix.localtime 1000. : Unix.tm);
        let our_date, our_ofday =
          Time.to_date_ofday (Time.of_span_since_epoch (Time.Span.of_sec unix_time)) ~zone
        in
        require_equal
          [%here]
          (module String)
          (Date.to_string our_date)
          localtime_date_string;
        require_equal
          [%here]
          (module String)
          (Time.Ofday.to_string our_ofday)
          localtime_ofday_string))
;;
