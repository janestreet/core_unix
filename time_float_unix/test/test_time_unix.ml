open Core
open Expect_test_helpers_core
module Unix = Core_unix
module Time = Time_float_unix
open Time
open Exposed_for_tests

let of_sec_since_epoch sec = of_span_since_epoch (Span.of_sec sec)

let%expect_test "[to_string_iso8601] in zulu" =
  let s = to_string_iso8601_basic ~zone:Zone.utc (of_sec_since_epoch 12.345678) in
  printf !"%s" s;
  [%expect {| 1970-01-01T00:00:12.345678Z |}]
;;

let%expect_test "[to_string_iso8601] in local (which is set to NYC in tests)" =
  let s =
    to_string_iso8601_basic ~zone:(Lazy.force Zone.local) (of_sec_since_epoch 12.345678)
  in
  printf !"%s" s;
  [%expect {| 1969-12-31T19:00:12.345678-05:00 |}]
;;

module%test [@name "ensure_colon_in_offset"] _ = struct
  let gen_digit_string ~length =
    let open Quickcheck.Let_syntax in
    let%bind len = length in
    String.gen_with_length len Char.gen_digit
  ;;

  let%test_unit "add colon" =
    let quickcheck_generator = gen_digit_string ~length:(Int.gen_incl 3 4) in
    Quickcheck.test quickcheck_generator ~sexp_of:String.sexp_of_t ~f:(fun digits ->
      assert (not (String.mem digits ':'));
      let output = ensure_colon_in_offset digits in
      assert (Int.( = ) (String.count output ~f:(Char.( = ) ':')) 1);
      let prefix, suffix = String.lsplit2_exn output ~on:':' in
      assert (String.( <> ) prefix "");
      assert (Int.( >= ) (String.length suffix) (String.length prefix)))
  ;;

  let gen_offset_with_colon =
    let open Quickcheck.Generator in
    let gen_prefix = gen_digit_string ~length:(Int.gen_incl 1 2) in
    let gen_suffix = gen_digit_string ~length:(return 2) in
    tuple2 gen_prefix gen_suffix >>| fun (a, b) -> String.concat [ a; ":"; b ]
  ;;

  let%test_unit "do not add colon" =
    Quickcheck.test gen_offset_with_colon ~sexp_of:String.sexp_of_t ~f:(fun offset ->
      assert (Int.( = ) (String.count offset ~f:(Char.( = ) ':')) 1);
      assert (String.equal offset (ensure_colon_in_offset offset)))
  ;;
end

let%test_unit _ =
  let unzoned_sexp = Sexp.of_string "(2015-07-03 16:27:00)" in
  set_sexp_zone Zone.utc;
  let in_utc = t_of_sexp unzoned_sexp in
  set_sexp_zone (Zone.of_utc_offset ~hours:8);
  let in_plus8 = t_of_sexp unzoned_sexp in
  set_sexp_zone (Lazy.force Zone.local);
  [%test_result: Span.t] ~expect:(Span.of_hr 8.) (diff in_utc in_plus8)
;;

let%test _ =
  Set.equal
    (Set.of_list [ epoch ])
    (Set.t_of_sexp
       (Sexp.List [ Float.sexp_of_t (to_span_since_epoch epoch |> Span.to_sec) ]))
;;

let%test_unit _ =
  let expected_next_multiple ~base ~after ~interval =
    let rec loop at = if at > after then at else loop (add at interval) in
    loop base
  in
  List.iter
    ~f:(fun (since_base, interval) ->
      let base = epoch in
      let sec = Span.of_sec in
      let interval = sec interval in
      let after = add base (sec since_base) in
      let actual_next_multiple = next_multiple ~base ~after ~interval () in
      let expected_next_multiple = expected_next_multiple ~base ~after ~interval in
      let relativize time = diff time base in
      let times_are_close t1 t2 = Float.( < ) (Float.abs (Span.to_us (diff t1 t2))) 1. in
      if not (times_are_close actual_next_multiple expected_next_multiple)
      then
        failwiths
          "Time.next_multiple"
          ( since_base
          , interval
          , relativize expected_next_multiple
          , relativize actual_next_multiple )
          [%sexp_of: float * Span.t * Span.t * Span.t])
    [ 0., 1.
    ; 0.1, 1.
    ; 0.9, 1.
    ; 1., 1.
    ; 1.1, 1.
    ; 1.9, 1.
    ; 1000.1, 1.
    ; -1., 1.
    ; -1., 0.1
    ; 1., 0.2
    ; 1E-5, 1E-6
    ]
;;

module%test [@name "of_tm"] _ = struct
  let unix_epoch_t =
    of_date_ofday ~zone:Zone.utc (Date.create_exn ~y:1970 ~m:Jan ~d:1) (Ofday.create ())
  ;;

  let%test_unit _ =
    [%test_result: t]
      ~expect:unix_epoch_t
      (of_tm ~zone:(Lazy.force Zone.local) (Unix.localtime 0.))
  ;;

  let%test_unit _ =
    [%test_result: t] ~expect:unix_epoch_t (of_tm ~zone:Zone.utc (Unix.gmtime 0.))
  ;;
end

module%test [@name "format"] _ = struct
  let hkg = Time.Zone.find_exn "Asia/Hong_Kong"
  let ldn = Time.Zone.find_exn "Europe/London"
  let nyc = Time.Zone.find_exn "America/New_York"
  let zones = [ hkg; ldn; nyc ]

  let test_time time =
    print_endline (Time.to_string_abs time ~zone:Zone.utc);
    List.iter zones ~f:(fun zone ->
      print_endline (format time ~zone "%F %T" ^ " -- " ^ Time.Zone.name zone))
  ;;

  let time1 = of_string_abs "2015-01-01 10:00:00 Europe/London"
  let time2 = of_string_abs "2015-06-06 10:00:00 Europe/London"

  let%expect_test _ =
    test_time time1;
    [%expect
      {|
      2015-01-01 10:00:00.000000Z
      2015-01-01 18:00:00 -- Asia/Hong_Kong
      2015-01-01 10:00:00 -- Europe/London
      2015-01-01 05:00:00 -- America/New_York
      |}]
  ;;

  let%expect_test _ =
    test_time time2;
    [%expect
      {|
      2015-06-06 09:00:00.000000Z
      2015-06-06 17:00:00 -- Asia/Hong_Kong
      2015-06-06 10:00:00 -- Europe/London
      2015-06-06 05:00:00 -- America/New_York
      |}]
  ;;

  let list_of_transition = function
    | None -> []
    | Some (time, _) -> [ time ]
  ;;

  let transitions_of_time time zone =
    List.concat_map
      ~f:list_of_transition
      [ Zone.prev_clock_shift zone ~at_or_before:time
      ; Zone.next_clock_shift zone ~strictly_after:time
      ]
  ;;

  let times_around time =
    List.map [ -2.; -1.; 0.; 1.; 2. ] ~f:(fun min -> Time.add time (Time.Span.of_min min))
  ;;

  let test_transition time = List.iter (times_around time) ~f:test_time

  let test_transitions time zone =
    List.iter (transitions_of_time time zone) ~f:(fun time ->
      print_endline "";
      test_transition time)
  ;;

  let%expect_test _ =
    test_transitions time1 ldn;
    [%expect
      {|
      2014-10-26 00:58:00.000000Z
      2014-10-26 08:58:00 -- Asia/Hong_Kong
      2014-10-26 01:58:00 -- Europe/London
      2014-10-25 20:58:00 -- America/New_York
      2014-10-26 00:59:00.000000Z
      2014-10-26 08:59:00 -- Asia/Hong_Kong
      2014-10-26 01:59:00 -- Europe/London
      2014-10-25 20:59:00 -- America/New_York
      2014-10-26 01:00:00.000000Z
      2014-10-26 09:00:00 -- Asia/Hong_Kong
      2014-10-26 01:00:00 -- Europe/London
      2014-10-25 21:00:00 -- America/New_York
      2014-10-26 01:01:00.000000Z
      2014-10-26 09:01:00 -- Asia/Hong_Kong
      2014-10-26 01:01:00 -- Europe/London
      2014-10-25 21:01:00 -- America/New_York
      2014-10-26 01:02:00.000000Z
      2014-10-26 09:02:00 -- Asia/Hong_Kong
      2014-10-26 01:02:00 -- Europe/London
      2014-10-25 21:02:00 -- America/New_York

      2015-03-29 00:58:00.000000Z
      2015-03-29 08:58:00 -- Asia/Hong_Kong
      2015-03-29 00:58:00 -- Europe/London
      2015-03-28 20:58:00 -- America/New_York
      2015-03-29 00:59:00.000000Z
      2015-03-29 08:59:00 -- Asia/Hong_Kong
      2015-03-29 00:59:00 -- Europe/London
      2015-03-28 20:59:00 -- America/New_York
      2015-03-29 01:00:00.000000Z
      2015-03-29 09:00:00 -- Asia/Hong_Kong
      2015-03-29 02:00:00 -- Europe/London
      2015-03-28 21:00:00 -- America/New_York
      2015-03-29 01:01:00.000000Z
      2015-03-29 09:01:00 -- Asia/Hong_Kong
      2015-03-29 02:01:00 -- Europe/London
      2015-03-28 21:01:00 -- America/New_York
      2015-03-29 01:02:00.000000Z
      2015-03-29 09:02:00 -- Asia/Hong_Kong
      2015-03-29 02:02:00 -- Europe/London
      2015-03-28 21:02:00 -- America/New_York
      |}]
  ;;

  let%expect_test _ =
    test_transitions time1 nyc;
    [%expect
      {|
      2014-11-02 05:58:00.000000Z
      2014-11-02 13:58:00 -- Asia/Hong_Kong
      2014-11-02 05:58:00 -- Europe/London
      2014-11-02 01:58:00 -- America/New_York
      2014-11-02 05:59:00.000000Z
      2014-11-02 13:59:00 -- Asia/Hong_Kong
      2014-11-02 05:59:00 -- Europe/London
      2014-11-02 01:59:00 -- America/New_York
      2014-11-02 06:00:00.000000Z
      2014-11-02 14:00:00 -- Asia/Hong_Kong
      2014-11-02 06:00:00 -- Europe/London
      2014-11-02 01:00:00 -- America/New_York
      2014-11-02 06:01:00.000000Z
      2014-11-02 14:01:00 -- Asia/Hong_Kong
      2014-11-02 06:01:00 -- Europe/London
      2014-11-02 01:01:00 -- America/New_York
      2014-11-02 06:02:00.000000Z
      2014-11-02 14:02:00 -- Asia/Hong_Kong
      2014-11-02 06:02:00 -- Europe/London
      2014-11-02 01:02:00 -- America/New_York

      2015-03-08 06:58:00.000000Z
      2015-03-08 14:58:00 -- Asia/Hong_Kong
      2015-03-08 06:58:00 -- Europe/London
      2015-03-08 01:58:00 -- America/New_York
      2015-03-08 06:59:00.000000Z
      2015-03-08 14:59:00 -- Asia/Hong_Kong
      2015-03-08 06:59:00 -- Europe/London
      2015-03-08 01:59:00 -- America/New_York
      2015-03-08 07:00:00.000000Z
      2015-03-08 15:00:00 -- Asia/Hong_Kong
      2015-03-08 07:00:00 -- Europe/London
      2015-03-08 03:00:00 -- America/New_York
      2015-03-08 07:01:00.000000Z
      2015-03-08 15:01:00 -- Asia/Hong_Kong
      2015-03-08 07:01:00 -- Europe/London
      2015-03-08 03:01:00 -- America/New_York
      2015-03-08 07:02:00.000000Z
      2015-03-08 15:02:00 -- Asia/Hong_Kong
      2015-03-08 07:02:00 -- Europe/London
      2015-03-08 03:02:00 -- America/New_York
      |}]
  ;;
end

module%test [@name "parse"] _ = struct
  let unix_epoch_t =
    of_date_ofday ~zone:Zone.utc (Date.create_exn ~y:1970 ~m:Jan ~d:1) (Ofday.create ())
  ;;

  let%test_unit _ =
    [%test_result: t]
      ~expect:unix_epoch_t
      (parse ~zone:Zone.utc ~fmt:"%Y-%m-%d %H:%M:%S" "1970-01-01 00:00:00")
  ;;

  let%test_unit _ =
    [%test_result: t]
      ~expect:unix_epoch_t
      (parse
         ~zone:(Zone.find_exn "Asia/Hong_Kong")
         ~fmt:"%Y-%m-%d %H:%M:%S"
         "1970-01-01 08:00:00")
  ;;
end

let%expect_test "accept float instead of time/span/ofday for hash tables and hash sets" =
  let module Of_string (M : Sexpable.S1) = struct
    type t = string M.t [@@deriving sexp]
  end
  in
  let test (module M : Sexpable) string =
    print_s (M.sexp_of_t (M.t_of_sexp (Sexp.of_string string)))
  in
  test (module Time.Hash_set) {| (0 0.05 946746000 1381152600) |};
  [%expect
    {|
    ((1969-12-31 19:00:00.000000-05:00)
     (1969-12-31 19:00:00.050000-05:00)
     (2000-01-01 12:00:00.000000-05:00)
     (2013-10-07 09:30:00.000000-04:00))
    |}];
  test
    (module Of_string (Time.Table))
    {|
    ((0          "arbitrary value")
     (0.05       "arbitrary value")
     (946746000  "arbitrary value")
     (1381152600 "arbitrary value")) |};
  [%expect
    {|
    (((1969-12-31 19:00:00.000000-05:00) "arbitrary value")
     ((1969-12-31 19:00:00.050000-05:00) "arbitrary value")
     ((2000-01-01 12:00:00.000000-05:00) "arbitrary value")
     ((2013-10-07 09:30:00.000000-04:00) "arbitrary value"))
    |}];
  test (module Time.Span.Hash_set) {| (0 1E-09 1E-06 0.001 1 60 3600 86400) |};
  [%expect {| (0s 1ns 1us 1ms 1s 1m 1h 1d) |}];
  test
    (module Of_string (Time.Span.Table))
    {|
    ((0     "arbitrary value")
     (1E-09 "arbitrary value")
     (1E-06 "arbitrary value")
     (0.001 "arbitrary value")
     (1     "arbitrary value")
     (60    "arbitrary value")
     (3600  "arbitrary value")
     (86400 "arbitrary value")) |};
  [%expect
    {|
    ((0s  "arbitrary value")
     (1ns "arbitrary value")
     (1us "arbitrary value")
     (1ms "arbitrary value")
     (1s  "arbitrary value")
     (1m  "arbitrary value")
     (1h  "arbitrary value")
     (1d  "arbitrary value"))
    |}];
  test (module Time.Ofday.Hash_set) {| (0 0.05 34200 43200) |};
  [%expect {| (00:00:00.000000 00:00:00.050000 09:30:00.000000 12:00:00.000000) |}];
  test
    (module Of_string (Time.Ofday.Table))
    {|
    ((0     "arbitrary value")
     (0.05  "arbitrary value")
     (34200 "arbitrary value")
     (43200 "arbitrary value")) |};
  [%expect
    {|
    ((00:00:00.000000 "arbitrary value")
     (00:00:00.050000 "arbitrary value")
     (09:30:00.000000 "arbitrary value")
     (12:00:00.000000 "arbitrary value"))
    |}]
;;

module%test [@name "Time robustly compare"] _ = struct
  let%test _ = of_sec_since_epoch 0.0 =. of_sec_since_epoch 0.000_000_99
  let%test _ = of_sec_since_epoch 0.0 <. of_sec_since_epoch 0.000_001_1

  let%test_unit _ =
    for i = 0 to 100 do
      let time = of_sec_since_epoch (Float.of_int i /. 17.) in
      assert (time =. (sexp_of_t time |> t_of_sexp))
    done
  ;;
end

let%expect_test "in tests, [to_string] uses NYC's time zone" =
  printf "%s" (to_string epoch);
  [%expect {| 1969-12-31 19:00:00.000000-05:00 |}]
;;

let%expect_test "in tests, [sexp_of_t] uses NYC's time zone" =
  printf !"%{Sexp}" [%sexp (epoch : t)];
  [%expect {| (1969-12-31 19:00:00.000000-05:00) |}]
;;

module _ = struct
  open Time.Ofday.Zoned

  let ( = ) = [%compare.equal: With_nonchronological_compare.t]

  let%test_unit _ =
    List.iter [ "12:00 nyc"; "12:00 America/New_York" ] ~f:(fun string ->
      let t = of_string string in
      assert (t = of_string (to_string t));
      assert (t = t_of_sexp (sexp_of_t t));
      assert (t = of_string (to_string_trimmed t));
      assert (
        Int.( <= ) (String.length (to_string_trimmed t)) (String.length (to_string t))))
  ;;
end

let%expect_test "our gmtime matches Unix.gmtime" =
  let unix_date_ofday (sec_since_epoch : float) =
    let parts = Float.modf sec_since_epoch in
    let sec = Float.Parts.integral parts in
    let subsec = Float.Parts.fractional parts in
    let sec, subsec =
      if Float.( < ) subsec 0. then sec -. 1., 1. +. subsec else sec, subsec
    in
    let tm = Unix.gmtime sec in
    let unix_date =
      Date.create_exn
        ~y:(tm.tm_year + 1900)
        ~m:(Month.of_int_exn (tm.tm_mon + 1))
        ~d:tm.tm_mday
    in
    let integral_ofday =
      (tm.tm_hour * 60 * 60) + (tm.tm_min * 60) + tm.tm_sec |> Float.of_int
    in
    let unix_ofday =
      (* put back the subseconds *)
      integral_ofday +. Float.abs subsec
      |> Time.Span.of_sec
      |> Time.Ofday.of_span_since_start_of_day_exn
    in
    unix_date, unix_ofday
  in
  let generator =
    let open Quickcheck.Generator.Let_syntax in
    let one_hundred_years = 86_400 * 365 * 100 * 1_000 * 1_000 in
    let upper_bound = one_hundred_years in
    let lower_bound = Int.neg one_hundred_years in
    let%map mics = Int.gen_incl lower_bound upper_bound in
    Float.of_int mics /. (1_000. *. 1_000.)
  in
  let gmtime time = Time.to_date_ofday ~zone:Time.Zone.utc time in
  Quickcheck.test
    generator
    ~sexp_of:[%sexp_of: float]
    ~trials:100_000
    ~examples:[ 0.; 100.; -100.; 86_400.; -86_400.; 90_000.; -90_000. ]
    ~f:(fun sec_since_epoch ->
      let time = Time.of_span_since_epoch (Time.Span.of_sec sec_since_epoch) in
      let my_date, my_ofday = gmtime time in
      let unix_date, unix_ofday = unix_date_ofday sec_since_epoch in
      let results = (my_date, my_ofday), (unix_date, unix_ofday) in
      if not
           (Tuple.T2.equal
              ~eq1:Date.equal
              ~eq2:Time.Ofday.equal
              (fst results)
              (snd results))
      then
        raise_s
          [%message
            "our gmtime doesn't match Unix.gmtime"
              (sec_since_epoch : float)
              (results : (Date.t * Time.Ofday.t) * (Date.t * Time.Ofday.t))])
;;

(* we expose the private type of Timish things to help the compiler optimize things
   like records of all floats.  This is not exactly an expect test in that we expect
   compilation to simply fail rather than a runtime test failure. *)
let%expect_test "time/span/ofday can be cast to their underlying type" =
  let _ = (Time.epoch :> float) in
  let _ = (Time.Span.zero :> float) in
  let _ = (Time.Ofday.start_of_day :> float) in
  ()
;;

let%expect_test "end-of-day constants" =
  let zones =
    List.map (Atomic.get Time.Zone.likely_machine_zones) ~f:Time.Zone.find_exn
  in
  let test_round_trip zone date ofday ~expect =
    require_equal
      (module Date)
      (Time.of_date_ofday ~zone date ofday |> Time.to_date ~zone)
      expect
      ~message:(Time.Zone.name zone)
  in
  let test date_string =
    let date = Date.of_string date_string in
    List.iter zones ~f:(fun zone ->
      test_round_trip zone date Time.Ofday.approximate_end_of_day ~expect:date;
      test_round_trip
        zone
        date
        Time.Ofday.start_of_next_day
        ~expect:(Date.add_days date 1))
  in
  test "1970-01-01";
  test "2013-10-07";
  test "2099-12-31";
  test "2121-04-01";
  [%expect {| |}]
;;

module Specialize_to_int (Poly : Stable1) = struct
  type t = int Poly.t [@@deriving bin_io, compare, sexp]
end

module%test [@name "Time.Stable"] _ = struct
  let zone_new_york = Zone.find_exn "America/New_York"

  let date_examples =
    [ "1912-06-23" (* before epoch *)
    ; "1970-01-01" (* date of epoch *)
    ; "1999-12-31" (* right before 19xx->20xx millennium shift *)
    ; "2000-01-01" (* right after 19xx->20xx millennium shift *)
    ; "2013-10-07" (* date during JS's history *)
    ; "2222-11-22" (* far future date *)
    ]
    |> List.map ~f:Date.of_string
  ;;

  let ofday_examples =
    (* significant day-boundary defined constants *)
    [ Ofday.start_of_day
    ; Ofday.approximate_end_of_day
    ; Ofday.start_of_next_day (* noon *)
    ; Ofday.create ~hr:12 () (* single units, down to microseconds *)
    ; Ofday.create ~us:1 ()
    ; Ofday.create ~ms:1 ()
    ; Ofday.create ~sec:1 ()
    ; Ofday.create ~min:1 ()
    ; Ofday.create ~hr:1 () (* some value in each unit, down to microseconds *)
    ; Ofday.create ~hr:13 ~min:42 ~sec:23 ~ms:622 ~us:933 ()
    ]
  ;;

  (* We construct and test lots of values because the stability of Time conversions is
       complex, and depends a lot on how comprehensive these tests are. *)
  let examples =
    let constructed_examples =
      List.concat_map date_examples ~f:(fun date ->
        List.map ofday_examples ~f:(fun ofday ->
          of_date_ofday ~zone:zone_new_york date ofday))
      |> List.dedup_and_sort ~compare:Time.compare
    in
    let pseudo_random_examples =
      let state = Random.State.make [| 1; 6; 1; 8; 0; 3; 3; 9; 8; 8; 7; 5 |] in
      List.init 10 ~f:(fun _ ->
        (* any digits further and the times fail to sexpify due to [gmtime] bounds *)
        Random.State.float_range state (-1e10) 1e10 |> Span.of_sec |> of_span_since_epoch)
      |> List.dedup_and_sort ~compare:Time.compare
    in
    constructed_examples @ pseudo_random_examples
  ;;

  let set_examples =
    [ Set.empty ] @ List.map examples ~f:Set.singleton @ [ Set.of_list examples ]
  ;;

  let map_examples =
    [ Map.empty ]
    @ List.mapi examples ~f:(fun i example -> Map.singleton example i)
    @ [ Map.of_alist_exn (List.mapi examples ~f:(fun i example -> example, i)) ]
  ;;

  module type S = sig
    type t [@@deriving bin_io, compare, sexp]

    val examples : t list
    val quickcheck_generator : t Quickcheck.Generator.t
  end

  let test_stability (module M : S) =
    require_does_not_raise (fun () ->
      (* For the pre-written examples, test round-tripping, and also print out the
           converted values so we will see if they change. *)
      print_and_check_stable_type (module M) M.examples;
      (* Test lots more pseudo-randomly generated examples for round-tripping. Do not
           print them out, as we don't want to read thousands of examples, so we won't
           know if their representation changes, but at least we will know they
           round-trip. *)
      quickcheck M.quickcheck_generator ~sexp_of:M.sexp_of_t ~f:(fun example ->
        require_does_not_raise (fun () ->
          let sexp = M.sexp_of_t example in
          let sexp_round_trip = M.t_of_sexp sexp in
          require_compare_equal (module M) example sexp_round_trip;
          let string = Binable.to_string (module M) example in
          let binio_round_trip = Binable.of_string (module M) string in
          require_compare_equal (module M) example binio_round_trip)))
  ;;

  module For_time = struct
    let compare x y = Time.robustly_compare x y
    let examples = examples

    let quickcheck_generator =
      (* If we add another digit, the times start to exceed the range of [gmtime] and
           sexp conversion raises. *)
      Int64.gen_uniform_incl (-10_000_000_000_000_000L) 10_000_000_000_000_000L
      (* We generate in units of microseconds because our current sexp representation is
           no more precise than that. *)
      |> Quickcheck.Generator.map ~f:(fun int64 ->
        Time.of_span_since_epoch (Span.of_us (Int64.to_float int64)))
    ;;
  end

  module For_set = struct
    let compare x y =
      List.compare For_time.compare (Core.Set.to_list x) (Core.Set.to_list y)
    ;;

    let examples = set_examples

    let quickcheck_generator =
      Quickcheck.Generator.map
        (List.quickcheck_generator For_time.quickcheck_generator)
        ~f:Time.Set.of_list
    ;;
  end

  module For_map = struct
    let compare x y =
      List.compare
        (Tuple2.compare ~cmp1:For_time.compare ~cmp2:Int.compare)
        (Core.Map.to_alist x)
        (Core.Map.to_alist y)
    ;;

    let examples = map_examples

    let quickcheck_generator =
      List.quickcheck_generator
        (Quickcheck.Generator.tuple2
           For_time.quickcheck_generator
           Int.quickcheck_generator)
      |> Quickcheck.Generator.filter_map ~f:(fun alist ->
        match Time.Map.of_alist alist with
        | `Ok map -> Some map
        | `Duplicate_key _ -> None)
    ;;
  end

  let%expect_test "V1" =
    test_stability
      (module struct
        include Time.Stable.V1
        include For_time
      end);
    [%expect
      {|
      (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
      ((sexp (1912-06-23 00:00:00.000000-05:00))
       (bin_io "\000\000\000\140\241\012\219\193"))
      ((sexp (1912-06-23 00:00:00.000001-05:00))
       (bin_io "\252\255\255\139\241\012\219\193"))
      ((sexp (1912-06-23 00:00:00.001000-05:00))
       (bin_io "\158\239\255\139\241\012\219\193"))
      ((sexp (1912-06-23 00:00:01.000000-05:00))
       (bin_io "\000\000\192\139\241\012\219\193"))
      ((sexp (1912-06-23 00:01:00.000000-05:00))
       (bin_io "\000\000\000}\241\012\219\193"))
      ((sexp (1912-06-23 01:00:00.000000-05:00))
       (bin_io "\000\000\000\b\238\012\219\193"))
      ((sexp (1912-06-23 12:00:00.000000-05:00))
       (bin_io "\000\000\000\\\199\012\219\193"))
      ((sexp (1912-06-23 13:42:23.622933-05:00))
       (bin_io "\222!\024\\\193\012\219\193"))
      ((sexp (1912-06-23 23:59:59.999999-05:00))
       (bin_io "\004\000\000,\157\012\219\193"))
      ((sexp (1912-06-24 00:00:00.000000-05:00))
       (bin_io "\000\000\000,\157\012\219\193"))
      ((sexp (1970-01-01 00:00:00.000000-05:00))
       (bin_io "\000\000\000\000\000\148\209@"))
      ((sexp (1970-01-01 00:00:00.000001-05:00))
       (bin_io "\1901\004\000\000\148\209@"))
      ((sexp (1970-01-01 00:00:00.001000-05:00)) (bin_io "\211Mb\016\000\148\209@"))
      ((sexp (1970-01-01 00:00:01.000000-05:00))
       (bin_io "\000\000\000\000@\148\209@"))
      ((sexp (1970-01-01 00:01:00.000000-05:00))
       (bin_io "\000\000\000\000\000\163\209@"))
      ((sexp (1970-01-01 01:00:00.000000-05:00))
       (bin_io "\000\000\000\000\000\024\213@"))
      ((sexp (1970-01-01 12:00:00.000000-05:00))
       (bin_io "\000\000\000\000\000\226\237@"))
      ((sexp (1970-01-01 13:42:23.622933-05:00))
       (bin_io "\234\151\136\247\249p\240@"))
      ((sexp (1970-01-01 23:59:59.999999-05:00))
       (bin_io "\145\243\254\255\255|\249@"))
      ((sexp (1970-01-02 00:00:00.000000-05:00))
       (bin_io "\000\000\000\000\000}\249@"))
      ((sexp (1999-12-31 00:00:00.000000-05:00)) (bin_io "\000\000\000(\0286\204A"))
      ((sexp (1999-12-31 00:00:00.000001-05:00)) (bin_io "\b\000\000(\0286\204A"))
      ((sexp (1999-12-31 00:00:00.001000-05:00)) (bin_io "\197 \000(\0286\204A"))
      ((sexp (1999-12-31 00:00:01.000000-05:00)) (bin_io "\000\000\128(\0286\204A"))
      ((sexp (1999-12-31 00:01:00.000000-05:00)) (bin_io "\000\000\000F\0286\204A"))
      ((sexp (1999-12-31 01:00:00.000000-05:00)) (bin_io "\000\000\0000#6\204A"))
      ((sexp (1999-12-31 12:00:00.000000-05:00)) (bin_io "\000\000\000\136p6\204A"))
      ((sexp (1999-12-31 13:42:23.622933-05:00)) (bin_io "E\188\207\135|6\204A"))
      ((sexp (1999-12-31 23:59:59.999999-05:00))
       (bin_io "\248\255\255\231\1966\204A"))
      ((sexp (2000-01-01 00:00:00.000000-05:00))
       (bin_io "\000\000\000\232\1966\204A"))
      ((sexp (2000-01-01 00:00:00.000001-05:00))
       (bin_io "\b\000\000\232\1966\204A"))
      ((sexp (2000-01-01 00:00:00.001000-05:00)) (bin_io "\197 \000\232\1966\204A"))
      ((sexp (2000-01-01 00:00:01.000000-05:00))
       (bin_io "\000\000\128\232\1966\204A"))
      ((sexp (2000-01-01 00:01:00.000000-05:00))
       (bin_io "\000\000\000\006\1976\204A"))
      ((sexp (2000-01-01 01:00:00.000000-05:00))
       (bin_io "\000\000\000\240\2036\204A"))
      ((sexp (2000-01-01 12:00:00.000000-05:00)) (bin_io "\000\000\000H\0257\204A"))
      ((sexp (2000-01-01 13:42:23.622933-05:00)) (bin_io "E\188\207G%7\204A"))
      ((sexp (2000-01-01 23:59:59.999999-05:00)) (bin_io "\248\255\255\167m7\204A"))
      ((sexp (2000-01-02 00:00:00.000000-05:00)) (bin_io "\000\000\000\168m7\204A"))
      ((sexp (2013-10-07 00:00:00.000000-04:00))
       (bin_io "\000\000\000p\140\148\212A"))
      ((sexp (2013-10-07 00:00:00.000001-04:00))
       (bin_io "\004\000\000p\140\148\212A"))
      ((sexp (2013-10-07 00:00:00.001000-04:00)) (bin_io "b\016\000p\140\148\212A"))
      ((sexp (2013-10-07 00:00:01.000000-04:00)) (bin_io "\000\000@p\140\148\212A"))
      ((sexp (2013-10-07 00:01:00.000000-04:00))
       (bin_io "\000\000\000\127\140\148\212A"))
      ((sexp (2013-10-07 01:00:00.000000-04:00))
       (bin_io "\000\000\000\244\143\148\212A"))
      ((sexp (2013-10-07 12:00:00.000000-04:00))
       (bin_io "\000\000\000\160\182\148\212A"))
      ((sexp (2013-10-07 13:42:23.622933-04:00))
       (bin_io "\"\222\231\159\188\148\212A"))
      ((sexp (2013-10-07 23:59:59.999999-04:00))
       (bin_io "\252\255\255\207\224\148\212A"))
      ((sexp (2013-10-08 00:00:00.000000-04:00))
       (bin_io "\000\000\000\208\224\148\212A"))
      ((sexp (2222-11-22 00:00:00.000000-05:00))
       (bin_io "\000\000\000\181\189\186\253A"))
      ((sexp (2222-11-22 00:00:00.000001-05:00))
       (bin_io "\001\000\000\181\189\186\253A"))
      ((sexp (2222-11-22 00:00:00.001000-05:00))
       (bin_io "\025\004\000\181\189\186\253A"))
      ((sexp (2222-11-22 00:00:01.000000-05:00))
       (bin_io "\000\000\016\181\189\186\253A"))
      ((sexp (2222-11-22 00:01:00.000000-05:00))
       (bin_io "\000\000\192\184\189\186\253A"))
      ((sexp (2222-11-22 01:00:00.000000-05:00))
       (bin_io "\000\000\000\150\190\186\253A"))
      ((sexp (2222-11-22 12:00:00.000000-05:00))
       (bin_io "\000\000\000A\200\186\253A"))
      ((sexp (2222-11-22 13:42:23.622933-05:00))
       (bin_io "\137\247\249\192\201\186\253A"))
      ((sexp (2222-11-22 23:59:59.999999-05:00))
       (bin_io "\255\255\255\204\210\186\253A"))
      ((sexp (2222-11-23 00:00:00.000000-05:00))
       (bin_io "\000\000\000\205\210\186\253A"))
      ((sexp (1675-11-17 16:21:05.905443-04:56:02))
       (bin_io "\167\193`=\199I\001\194"))
      ((sexp (1694-11-02 23:39:55.669968-04:56:02))
       (bin_io "\232\163\146\234v,\000\194"))
      ((sexp (1765-07-10 01:22:43.148142-04:56:02))
       (bin_io "6\161\173\019\192\t\248\193"))
      ((sexp (1801-06-12 14:02:01.068971-04:56:02))
       (bin_io "\127\229N\244\166\208\243\193"))
      ((sexp (1885-03-31 22:46:16.830498-05:00)) (bin_io "\144l\229n3\237\227\193"))
      ((sexp (1998-08-26 10:41:51.551338-04:00)) (bin_io "@\146\198W\015\242\202A"))
      ((sexp (2037-10-03 11:20:43.521111-04:00))
       (bin_io "\224Y\225\018\146\220\223A"))
      ((sexp (2076-09-15 07:19:49.513504-05:00)) (bin_io "\160n\176L\205\022\233A"))
      ((sexp (2081-01-03 17:06:45.819349-05:00))
       (bin_io "\0288\186\014\195\025\234A"))
      ((sexp (2175-12-01 10:47:07.694916-05:00)) (bin_io "`\030\187\183\0215\248A"))
      |}];
    (* test that t_of_sexp accepts sexps qualified with time zones in two formats *)
    let test string =
      require_does_not_raise (fun () ->
        print_s
          [%sexp (Time.Stable.V1.t_of_sexp (Sexp.of_string string) : Time.Stable.V1.t)])
    in
    test "(2012-04-09 12:00:00.000000-04:00:00)";
    test "(2012-04-09 12:00:00.000000 America/New_York)";
    [%expect
      {|
      (2012-04-09 12:00:00.000000-04:00)
      (2012-04-09 12:00:00.000000-04:00)
      |}];
    (* test that t_of_sexp accepts leap seconds
         NB. there are such things as non-integer hour offset timezones, so we need to be
         able to accept leap seconds even when the minutes aren't zero. This obviously
         isn't such a timezone, but the output sexp will be in UTC-4, so the input sexp
         should be so as well so that the result is obviously as intended.
    *)
    test "(2012-04-09 05:14:60.000000-04:00)";
    test "(2012-04-09 05:14:60.123456-04:00)";
    [%expect
      {|
      (2012-04-09 05:15:00.000000-04:00)
      (2012-04-09 05:15:00.000000-04:00)
      |}]
  ;;

  let%expect_test "V1.Set" =
    test_stability
      (module struct
        include Time.Stable.V1.Set
        include For_set
      end);
    [%expect
      {|
      (bin_shape_digest 4e7cbf6fe56bd628b963b7f8259e58bf)
      ((sexp ()) (bin_io "\000"))
      ((sexp ((1912-06-23 00:00:00.000000-05:00)))
       (bin_io "\001\000\000\000\140\241\012\219\193"))
      ((sexp ((1912-06-23 00:00:00.000001-05:00)))
       (bin_io "\001\252\255\255\139\241\012\219\193"))
      ((sexp ((1912-06-23 00:00:00.001000-05:00)))
       (bin_io "\001\158\239\255\139\241\012\219\193"))
      ((sexp ((1912-06-23 00:00:01.000000-05:00)))
       (bin_io "\001\000\000\192\139\241\012\219\193"))
      ((sexp ((1912-06-23 00:01:00.000000-05:00)))
       (bin_io "\001\000\000\000}\241\012\219\193"))
      ((sexp ((1912-06-23 01:00:00.000000-05:00)))
       (bin_io "\001\000\000\000\b\238\012\219\193"))
      ((sexp ((1912-06-23 12:00:00.000000-05:00)))
       (bin_io "\001\000\000\000\\\199\012\219\193"))
      ((sexp ((1912-06-23 13:42:23.622933-05:00)))
       (bin_io "\001\222!\024\\\193\012\219\193"))
      ((sexp ((1912-06-23 23:59:59.999999-05:00)))
       (bin_io "\001\004\000\000,\157\012\219\193"))
      ((sexp ((1912-06-24 00:00:00.000000-05:00)))
       (bin_io "\001\000\000\000,\157\012\219\193"))
      ((sexp ((1970-01-01 00:00:00.000000-05:00)))
       (bin_io "\001\000\000\000\000\000\148\209@"))
      ((sexp ((1970-01-01 00:00:00.000001-05:00)))
       (bin_io "\001\1901\004\000\000\148\209@"))
      ((sexp ((1970-01-01 00:00:00.001000-05:00)))
       (bin_io "\001\211Mb\016\000\148\209@"))
      ((sexp ((1970-01-01 00:00:01.000000-05:00)))
       (bin_io "\001\000\000\000\000@\148\209@"))
      ((sexp ((1970-01-01 00:01:00.000000-05:00)))
       (bin_io "\001\000\000\000\000\000\163\209@"))
      ((sexp ((1970-01-01 01:00:00.000000-05:00)))
       (bin_io "\001\000\000\000\000\000\024\213@"))
      ((sexp ((1970-01-01 12:00:00.000000-05:00)))
       (bin_io "\001\000\000\000\000\000\226\237@"))
      ((sexp ((1970-01-01 13:42:23.622933-05:00)))
       (bin_io "\001\234\151\136\247\249p\240@"))
      ((sexp ((1970-01-01 23:59:59.999999-05:00)))
       (bin_io "\001\145\243\254\255\255|\249@"))
      ((sexp ((1970-01-02 00:00:00.000000-05:00)))
       (bin_io "\001\000\000\000\000\000}\249@"))
      ((sexp ((1999-12-31 00:00:00.000000-05:00)))
       (bin_io "\001\000\000\000(\0286\204A"))
      ((sexp ((1999-12-31 00:00:00.000001-05:00)))
       (bin_io "\001\b\000\000(\0286\204A"))
      ((sexp ((1999-12-31 00:00:00.001000-05:00)))
       (bin_io "\001\197 \000(\0286\204A"))
      ((sexp ((1999-12-31 00:00:01.000000-05:00)))
       (bin_io "\001\000\000\128(\0286\204A"))
      ((sexp ((1999-12-31 00:01:00.000000-05:00)))
       (bin_io "\001\000\000\000F\0286\204A"))
      ((sexp ((1999-12-31 01:00:00.000000-05:00)))
       (bin_io "\001\000\000\0000#6\204A"))
      ((sexp ((1999-12-31 12:00:00.000000-05:00)))
       (bin_io "\001\000\000\000\136p6\204A"))
      ((sexp ((1999-12-31 13:42:23.622933-05:00)))
       (bin_io "\001E\188\207\135|6\204A"))
      ((sexp ((1999-12-31 23:59:59.999999-05:00)))
       (bin_io "\001\248\255\255\231\1966\204A"))
      ((sexp ((2000-01-01 00:00:00.000000-05:00)))
       (bin_io "\001\000\000\000\232\1966\204A"))
      ((sexp ((2000-01-01 00:00:00.000001-05:00)))
       (bin_io "\001\b\000\000\232\1966\204A"))
      ((sexp ((2000-01-01 00:00:00.001000-05:00)))
       (bin_io "\001\197 \000\232\1966\204A"))
      ((sexp ((2000-01-01 00:00:01.000000-05:00)))
       (bin_io "\001\000\000\128\232\1966\204A"))
      ((sexp ((2000-01-01 00:01:00.000000-05:00)))
       (bin_io "\001\000\000\000\006\1976\204A"))
      ((sexp ((2000-01-01 01:00:00.000000-05:00)))
       (bin_io "\001\000\000\000\240\2036\204A"))
      ((sexp ((2000-01-01 12:00:00.000000-05:00)))
       (bin_io "\001\000\000\000H\0257\204A"))
      ((sexp ((2000-01-01 13:42:23.622933-05:00))) (bin_io "\001E\188\207G%7\204A"))
      ((sexp ((2000-01-01 23:59:59.999999-05:00)))
       (bin_io "\001\248\255\255\167m7\204A"))
      ((sexp ((2000-01-02 00:00:00.000000-05:00)))
       (bin_io "\001\000\000\000\168m7\204A"))
      ((sexp ((2013-10-07 00:00:00.000000-04:00)))
       (bin_io "\001\000\000\000p\140\148\212A"))
      ((sexp ((2013-10-07 00:00:00.000001-04:00)))
       (bin_io "\001\004\000\000p\140\148\212A"))
      ((sexp ((2013-10-07 00:00:00.001000-04:00)))
       (bin_io "\001b\016\000p\140\148\212A"))
      ((sexp ((2013-10-07 00:00:01.000000-04:00)))
       (bin_io "\001\000\000@p\140\148\212A"))
      ((sexp ((2013-10-07 00:01:00.000000-04:00)))
       (bin_io "\001\000\000\000\127\140\148\212A"))
      ((sexp ((2013-10-07 01:00:00.000000-04:00)))
       (bin_io "\001\000\000\000\244\143\148\212A"))
      ((sexp ((2013-10-07 12:00:00.000000-04:00)))
       (bin_io "\001\000\000\000\160\182\148\212A"))
      ((sexp ((2013-10-07 13:42:23.622933-04:00)))
       (bin_io "\001\"\222\231\159\188\148\212A"))
      ((sexp ((2013-10-07 23:59:59.999999-04:00)))
       (bin_io "\001\252\255\255\207\224\148\212A"))
      ((sexp ((2013-10-08 00:00:00.000000-04:00)))
       (bin_io "\001\000\000\000\208\224\148\212A"))
      ((sexp ((2222-11-22 00:00:00.000000-05:00)))
       (bin_io "\001\000\000\000\181\189\186\253A"))
      ((sexp ((2222-11-22 00:00:00.000001-05:00)))
       (bin_io "\001\001\000\000\181\189\186\253A"))
      ((sexp ((2222-11-22 00:00:00.001000-05:00)))
       (bin_io "\001\025\004\000\181\189\186\253A"))
      ((sexp ((2222-11-22 00:00:01.000000-05:00)))
       (bin_io "\001\000\000\016\181\189\186\253A"))
      ((sexp ((2222-11-22 00:01:00.000000-05:00)))
       (bin_io "\001\000\000\192\184\189\186\253A"))
      ((sexp ((2222-11-22 01:00:00.000000-05:00)))
       (bin_io "\001\000\000\000\150\190\186\253A"))
      ((sexp ((2222-11-22 12:00:00.000000-05:00)))
       (bin_io "\001\000\000\000A\200\186\253A"))
      ((sexp ((2222-11-22 13:42:23.622933-05:00)))
       (bin_io "\001\137\247\249\192\201\186\253A"))
      ((sexp ((2222-11-22 23:59:59.999999-05:00)))
       (bin_io "\001\255\255\255\204\210\186\253A"))
      ((sexp ((2222-11-23 00:00:00.000000-05:00)))
       (bin_io "\001\000\000\000\205\210\186\253A"))
      ((sexp ((1675-11-17 16:21:05.905443-04:56:02)))
       (bin_io "\001\167\193`=\199I\001\194"))
      ((sexp ((1694-11-02 23:39:55.669968-04:56:02)))
       (bin_io "\001\232\163\146\234v,\000\194"))
      ((sexp ((1765-07-10 01:22:43.148142-04:56:02)))
       (bin_io "\0016\161\173\019\192\t\248\193"))
      ((sexp ((1801-06-12 14:02:01.068971-04:56:02)))
       (bin_io "\001\127\229N\244\166\208\243\193"))
      ((sexp ((1885-03-31 22:46:16.830498-05:00)))
       (bin_io "\001\144l\229n3\237\227\193"))
      ((sexp ((1998-08-26 10:41:51.551338-04:00)))
       (bin_io "\001@\146\198W\015\242\202A"))
      ((sexp ((2037-10-03 11:20:43.521111-04:00)))
       (bin_io "\001\224Y\225\018\146\220\223A"))
      ((sexp ((2076-09-15 07:19:49.513504-05:00)))
       (bin_io "\001\160n\176L\205\022\233A"))
      ((sexp ((2081-01-03 17:06:45.819349-05:00)))
       (bin_io "\001\0288\186\014\195\025\234A"))
      ((sexp ((2175-12-01 10:47:07.694916-05:00)))
       (bin_io "\001`\030\187\183\0215\248A"))
      ((sexp (
         (1675-11-17 16:21:05.905443-04:56:02)
         (1694-11-02 23:39:55.669968-04:56:02)
         (1765-07-10 01:22:43.148142-04:56:02)
         (1801-06-12 14:02:01.068971-04:56:02)
         (1885-03-31 22:46:16.830498-05:00)
         (1912-06-23 00:00:00.000000-05:00)
         (1912-06-23 00:00:00.000001-05:00)
         (1912-06-23 00:00:00.001000-05:00)
         (1912-06-23 00:00:01.000000-05:00)
         (1912-06-23 00:01:00.000000-05:00)
         (1912-06-23 01:00:00.000000-05:00)
         (1912-06-23 12:00:00.000000-05:00)
         (1912-06-23 13:42:23.622933-05:00)
         (1912-06-23 23:59:59.999999-05:00)
         (1912-06-24 00:00:00.000000-05:00)
         (1970-01-01 00:00:00.000000-05:00)
         (1970-01-01 00:00:00.000001-05:00)
         (1970-01-01 00:00:00.001000-05:00)
         (1970-01-01 00:00:01.000000-05:00)
         (1970-01-01 00:01:00.000000-05:00)
         (1970-01-01 01:00:00.000000-05:00)
         (1970-01-01 12:00:00.000000-05:00)
         (1970-01-01 13:42:23.622933-05:00)
         (1970-01-01 23:59:59.999999-05:00)
         (1970-01-02 00:00:00.000000-05:00)
         (1998-08-26 10:41:51.551338-04:00)
         (1999-12-31 00:00:00.000000-05:00)
         (1999-12-31 00:00:00.000001-05:00)
         (1999-12-31 00:00:00.001000-05:00)
         (1999-12-31 00:00:01.000000-05:00)
         (1999-12-31 00:01:00.000000-05:00)
         (1999-12-31 01:00:00.000000-05:00)
         (1999-12-31 12:00:00.000000-05:00)
         (1999-12-31 13:42:23.622933-05:00)
         (1999-12-31 23:59:59.999999-05:00)
         (2000-01-01 00:00:00.000000-05:00)
         (2000-01-01 00:00:00.000001-05:00)
         (2000-01-01 00:00:00.001000-05:00)
         (2000-01-01 00:00:01.000000-05:00)
         (2000-01-01 00:01:00.000000-05:00)
         (2000-01-01 01:00:00.000000-05:00)
         (2000-01-01 12:00:00.000000-05:00)
         (2000-01-01 13:42:23.622933-05:00)
         (2000-01-01 23:59:59.999999-05:00)
         (2000-01-02 00:00:00.000000-05:00)
         (2013-10-07 00:00:00.000000-04:00)
         (2013-10-07 00:00:00.000001-04:00)
         (2013-10-07 00:00:00.001000-04:00)
         (2013-10-07 00:00:01.000000-04:00)
         (2013-10-07 00:01:00.000000-04:00)
         (2013-10-07 01:00:00.000000-04:00)
         (2013-10-07 12:00:00.000000-04:00)
         (2013-10-07 13:42:23.622933-04:00)
         (2013-10-07 23:59:59.999999-04:00)
         (2013-10-08 00:00:00.000000-04:00)
         (2037-10-03 11:20:43.521111-04:00)
         (2076-09-15 07:19:49.513504-05:00)
         (2081-01-03 17:06:45.819349-05:00)
         (2175-12-01 10:47:07.694916-05:00)
         (2222-11-22 00:00:00.000000-05:00)
         (2222-11-22 00:00:00.000001-05:00)
         (2222-11-22 00:00:00.001000-05:00)
         (2222-11-22 00:00:01.000000-05:00)
         (2222-11-22 00:01:00.000000-05:00)
         (2222-11-22 01:00:00.000000-05:00)
         (2222-11-22 12:00:00.000000-05:00)
         (2222-11-22 13:42:23.622933-05:00)
         (2222-11-22 23:59:59.999999-05:00)
         (2222-11-23 00:00:00.000000-05:00)))
       (bin_io
        "E\167\193`=\199I\001\194\232\163\146\234v,\000\1946\161\173\019\192\t\248\193\127\229N\244\166\208\243\193\144l\229n3\237\227\193\000\000\000\140\241\012\219\193\252\255\255\139\241\012\219\193\158\239\255\139\241\012\219\193\000\000\192\139\241\012\219\193\000\000\000}\241\012\219\193\000\000\000\b\238\012\219\193\000\000\000\\\199\012\219\193\222!\024\\\193\012\219\193\004\000\000,\157\012\219\193\000\000\000,\157\012\219\193\000\000\000\000\000\148\209@\1901\004\000\000\148\209@\211Mb\016\000\148\209@\000\000\000\000@\148\209@\000\000\000\000\000\163\209@\000\000\000\000\000\024\213@\000\000\000\000\000\226\237@\234\151\136\247\249p\240@\145\243\254\255\255|\249@\000\000\000\000\000}\249@@\146\198W\015\242\202A\000\000\000(\0286\204A\b\000\000(\0286\204A\197 \000(\0286\204A\000\000\128(\0286\204A\000\000\000F\0286\204A\000\000\0000#6\204A\000\000\000\136p6\204AE\188\207\135|6\204A\248\255\255\231\1966\204A\000\000\000\232\1966\204A\b\000\000\232\1966\204A\197 \000\232\1966\204A\000\000\128\232\1966\204A\000\000\000\006\1976\204A\000\000\000\240\2036\204A\000\000\000H\0257\204AE\188\207G%7\204A\248\255\255\167m7\204A\000\000\000\168m7\204A\000\000\000p\140\148\212A\004\000\000p\140\148\212Ab\016\000p\140\148\212A\000\000@p\140\148\212A\000\000\000\127\140\148\212A\000\000\000\244\143\148\212A\000\000\000\160\182\148\212A\"\222\231\159\188\148\212A\252\255\255\207\224\148\212A\000\000\000\208\224\148\212A\224Y\225\018\146\220\223A\160n\176L\205\022\233A\0288\186\014\195\025\234A`\030\187\183\0215\248A\000\000\000\181\189\186\253A\001\000\000\181\189\186\253A\025\004\000\181\189\186\253A\000\000\016\181\189\186\253A\000\000\192\184\189\186\253A\000\000\000\150\190\186\253A\000\000\000A\200\186\253A\137\247\249\192\201\186\253A\255\255\255\204\210\186\253A\000\000\000\205\210\186\253A"))
      |}]
  ;;

  let%expect_test "V1.Map" =
    test_stability
      (module struct
        include Specialize_to_int (Time.Stable.V1.Map)
        include For_map
      end);
    [%expect
      {|
      (bin_shape_digest 31404094f08cdbe1f9fca07a1a1e5303)
      ((sexp ()) (bin_io "\000"))
      ((sexp (((1912-06-23 00:00:00.000000-05:00) 0)))
       (bin_io "\001\000\000\000\140\241\012\219\193\000"))
      ((sexp (((1912-06-23 00:00:00.000001-05:00) 1)))
       (bin_io "\001\252\255\255\139\241\012\219\193\001"))
      ((sexp (((1912-06-23 00:00:00.001000-05:00) 2)))
       (bin_io "\001\158\239\255\139\241\012\219\193\002"))
      ((sexp (((1912-06-23 00:00:01.000000-05:00) 3)))
       (bin_io "\001\000\000\192\139\241\012\219\193\003"))
      ((sexp (((1912-06-23 00:01:00.000000-05:00) 4)))
       (bin_io "\001\000\000\000}\241\012\219\193\004"))
      ((sexp (((1912-06-23 01:00:00.000000-05:00) 5)))
       (bin_io "\001\000\000\000\b\238\012\219\193\005"))
      ((sexp (((1912-06-23 12:00:00.000000-05:00) 6)))
       (bin_io "\001\000\000\000\\\199\012\219\193\006"))
      ((sexp (((1912-06-23 13:42:23.622933-05:00) 7)))
       (bin_io "\001\222!\024\\\193\012\219\193\007"))
      ((sexp (((1912-06-23 23:59:59.999999-05:00) 8)))
       (bin_io "\001\004\000\000,\157\012\219\193\b"))
      ((sexp (((1912-06-24 00:00:00.000000-05:00) 9)))
       (bin_io "\001\000\000\000,\157\012\219\193\t"))
      ((sexp (((1970-01-01 00:00:00.000000-05:00) 10)))
       (bin_io "\001\000\000\000\000\000\148\209@\n"))
      ((sexp (((1970-01-01 00:00:00.000001-05:00) 11)))
       (bin_io "\001\1901\004\000\000\148\209@\011"))
      ((sexp (((1970-01-01 00:00:00.001000-05:00) 12)))
       (bin_io "\001\211Mb\016\000\148\209@\012"))
      ((sexp (((1970-01-01 00:00:01.000000-05:00) 13)))
       (bin_io "\001\000\000\000\000@\148\209@\r"))
      ((sexp (((1970-01-01 00:01:00.000000-05:00) 14)))
       (bin_io "\001\000\000\000\000\000\163\209@\014"))
      ((sexp (((1970-01-01 01:00:00.000000-05:00) 15)))
       (bin_io "\001\000\000\000\000\000\024\213@\015"))
      ((sexp (((1970-01-01 12:00:00.000000-05:00) 16)))
       (bin_io "\001\000\000\000\000\000\226\237@\016"))
      ((sexp (((1970-01-01 13:42:23.622933-05:00) 17)))
       (bin_io "\001\234\151\136\247\249p\240@\017"))
      ((sexp (((1970-01-01 23:59:59.999999-05:00) 18)))
       (bin_io "\001\145\243\254\255\255|\249@\018"))
      ((sexp (((1970-01-02 00:00:00.000000-05:00) 19)))
       (bin_io "\001\000\000\000\000\000}\249@\019"))
      ((sexp (((1999-12-31 00:00:00.000000-05:00) 20)))
       (bin_io "\001\000\000\000(\0286\204A\020"))
      ((sexp (((1999-12-31 00:00:00.000001-05:00) 21)))
       (bin_io "\001\b\000\000(\0286\204A\021"))
      ((sexp (((1999-12-31 00:00:00.001000-05:00) 22)))
       (bin_io "\001\197 \000(\0286\204A\022"))
      ((sexp (((1999-12-31 00:00:01.000000-05:00) 23)))
       (bin_io "\001\000\000\128(\0286\204A\023"))
      ((sexp (((1999-12-31 00:01:00.000000-05:00) 24)))
       (bin_io "\001\000\000\000F\0286\204A\024"))
      ((sexp (((1999-12-31 01:00:00.000000-05:00) 25)))
       (bin_io "\001\000\000\0000#6\204A\025"))
      ((sexp (((1999-12-31 12:00:00.000000-05:00) 26)))
       (bin_io "\001\000\000\000\136p6\204A\026"))
      ((sexp (((1999-12-31 13:42:23.622933-05:00) 27)))
       (bin_io "\001E\188\207\135|6\204A\027"))
      ((sexp (((1999-12-31 23:59:59.999999-05:00) 28)))
       (bin_io "\001\248\255\255\231\1966\204A\028"))
      ((sexp (((2000-01-01 00:00:00.000000-05:00) 29)))
       (bin_io "\001\000\000\000\232\1966\204A\029"))
      ((sexp (((2000-01-01 00:00:00.000001-05:00) 30)))
       (bin_io "\001\b\000\000\232\1966\204A\030"))
      ((sexp (((2000-01-01 00:00:00.001000-05:00) 31)))
       (bin_io "\001\197 \000\232\1966\204A\031"))
      ((sexp (((2000-01-01 00:00:01.000000-05:00) 32)))
       (bin_io "\001\000\000\128\232\1966\204A "))
      ((sexp (((2000-01-01 00:01:00.000000-05:00) 33)))
       (bin_io "\001\000\000\000\006\1976\204A!"))
      ((sexp (((2000-01-01 01:00:00.000000-05:00) 34)))
       (bin_io "\001\000\000\000\240\2036\204A\""))
      ((sexp (((2000-01-01 12:00:00.000000-05:00) 35)))
       (bin_io "\001\000\000\000H\0257\204A#"))
      ((sexp (((2000-01-01 13:42:23.622933-05:00) 36)))
       (bin_io "\001E\188\207G%7\204A$"))
      ((sexp (((2000-01-01 23:59:59.999999-05:00) 37)))
       (bin_io "\001\248\255\255\167m7\204A%"))
      ((sexp (((2000-01-02 00:00:00.000000-05:00) 38)))
       (bin_io "\001\000\000\000\168m7\204A&"))
      ((sexp (((2013-10-07 00:00:00.000000-04:00) 39)))
       (bin_io "\001\000\000\000p\140\148\212A'"))
      ((sexp (((2013-10-07 00:00:00.000001-04:00) 40)))
       (bin_io "\001\004\000\000p\140\148\212A("))
      ((sexp (((2013-10-07 00:00:00.001000-04:00) 41)))
       (bin_io "\001b\016\000p\140\148\212A)"))
      ((sexp (((2013-10-07 00:00:01.000000-04:00) 42)))
       (bin_io "\001\000\000@p\140\148\212A*"))
      ((sexp (((2013-10-07 00:01:00.000000-04:00) 43)))
       (bin_io "\001\000\000\000\127\140\148\212A+"))
      ((sexp (((2013-10-07 01:00:00.000000-04:00) 44)))
       (bin_io "\001\000\000\000\244\143\148\212A,"))
      ((sexp (((2013-10-07 12:00:00.000000-04:00) 45)))
       (bin_io "\001\000\000\000\160\182\148\212A-"))
      ((sexp (((2013-10-07 13:42:23.622933-04:00) 46)))
       (bin_io "\001\"\222\231\159\188\148\212A."))
      ((sexp (((2013-10-07 23:59:59.999999-04:00) 47)))
       (bin_io "\001\252\255\255\207\224\148\212A/"))
      ((sexp (((2013-10-08 00:00:00.000000-04:00) 48)))
       (bin_io "\001\000\000\000\208\224\148\212A0"))
      ((sexp (((2222-11-22 00:00:00.000000-05:00) 49)))
       (bin_io "\001\000\000\000\181\189\186\253A1"))
      ((sexp (((2222-11-22 00:00:00.000001-05:00) 50)))
       (bin_io "\001\001\000\000\181\189\186\253A2"))
      ((sexp (((2222-11-22 00:00:00.001000-05:00) 51)))
       (bin_io "\001\025\004\000\181\189\186\253A3"))
      ((sexp (((2222-11-22 00:00:01.000000-05:00) 52)))
       (bin_io "\001\000\000\016\181\189\186\253A4"))
      ((sexp (((2222-11-22 00:01:00.000000-05:00) 53)))
       (bin_io "\001\000\000\192\184\189\186\253A5"))
      ((sexp (((2222-11-22 01:00:00.000000-05:00) 54)))
       (bin_io "\001\000\000\000\150\190\186\253A6"))
      ((sexp (((2222-11-22 12:00:00.000000-05:00) 55)))
       (bin_io "\001\000\000\000A\200\186\253A7"))
      ((sexp (((2222-11-22 13:42:23.622933-05:00) 56)))
       (bin_io "\001\137\247\249\192\201\186\253A8"))
      ((sexp (((2222-11-22 23:59:59.999999-05:00) 57)))
       (bin_io "\001\255\255\255\204\210\186\253A9"))
      ((sexp (((2222-11-23 00:00:00.000000-05:00) 58)))
       (bin_io "\001\000\000\000\205\210\186\253A:"))
      ((sexp (((1675-11-17 16:21:05.905443-04:56:02) 59)))
       (bin_io "\001\167\193`=\199I\001\194;"))
      ((sexp (((1694-11-02 23:39:55.669968-04:56:02) 60)))
       (bin_io "\001\232\163\146\234v,\000\194<"))
      ((sexp (((1765-07-10 01:22:43.148142-04:56:02) 61)))
       (bin_io "\0016\161\173\019\192\t\248\193="))
      ((sexp (((1801-06-12 14:02:01.068971-04:56:02) 62)))
       (bin_io "\001\127\229N\244\166\208\243\193>"))
      ((sexp (((1885-03-31 22:46:16.830498-05:00) 63)))
       (bin_io "\001\144l\229n3\237\227\193?"))
      ((sexp (((1998-08-26 10:41:51.551338-04:00) 64)))
       (bin_io "\001@\146\198W\015\242\202A@"))
      ((sexp (((2037-10-03 11:20:43.521111-04:00) 65)))
       (bin_io "\001\224Y\225\018\146\220\223AA"))
      ((sexp (((2076-09-15 07:19:49.513504-05:00) 66)))
       (bin_io "\001\160n\176L\205\022\233AB"))
      ((sexp (((2081-01-03 17:06:45.819349-05:00) 67)))
       (bin_io "\001\0288\186\014\195\025\234AC"))
      ((sexp (((2175-12-01 10:47:07.694916-05:00) 68)))
       (bin_io "\001`\030\187\183\0215\248AD"))
      ((sexp (
         ((1675-11-17 16:21:05.905443-04:56:02) 59)
         ((1694-11-02 23:39:55.669968-04:56:02) 60)
         ((1765-07-10 01:22:43.148142-04:56:02) 61)
         ((1801-06-12 14:02:01.068971-04:56:02) 62)
         ((1885-03-31 22:46:16.830498-05:00)    63)
         ((1912-06-23 00:00:00.000000-05:00)    0)
         ((1912-06-23 00:00:00.000001-05:00)    1)
         ((1912-06-23 00:00:00.001000-05:00)    2)
         ((1912-06-23 00:00:01.000000-05:00)    3)
         ((1912-06-23 00:01:00.000000-05:00)    4)
         ((1912-06-23 01:00:00.000000-05:00)    5)
         ((1912-06-23 12:00:00.000000-05:00)    6)
         ((1912-06-23 13:42:23.622933-05:00)    7)
         ((1912-06-23 23:59:59.999999-05:00)    8)
         ((1912-06-24 00:00:00.000000-05:00)    9)
         ((1970-01-01 00:00:00.000000-05:00)    10)
         ((1970-01-01 00:00:00.000001-05:00)    11)
         ((1970-01-01 00:00:00.001000-05:00)    12)
         ((1970-01-01 00:00:01.000000-05:00)    13)
         ((1970-01-01 00:01:00.000000-05:00)    14)
         ((1970-01-01 01:00:00.000000-05:00)    15)
         ((1970-01-01 12:00:00.000000-05:00)    16)
         ((1970-01-01 13:42:23.622933-05:00)    17)
         ((1970-01-01 23:59:59.999999-05:00)    18)
         ((1970-01-02 00:00:00.000000-05:00)    19)
         ((1998-08-26 10:41:51.551338-04:00)    64)
         ((1999-12-31 00:00:00.000000-05:00)    20)
         ((1999-12-31 00:00:00.000001-05:00)    21)
         ((1999-12-31 00:00:00.001000-05:00)    22)
         ((1999-12-31 00:00:01.000000-05:00)    23)
         ((1999-12-31 00:01:00.000000-05:00)    24)
         ((1999-12-31 01:00:00.000000-05:00)    25)
         ((1999-12-31 12:00:00.000000-05:00)    26)
         ((1999-12-31 13:42:23.622933-05:00)    27)
         ((1999-12-31 23:59:59.999999-05:00)    28)
         ((2000-01-01 00:00:00.000000-05:00)    29)
         ((2000-01-01 00:00:00.000001-05:00)    30)
         ((2000-01-01 00:00:00.001000-05:00)    31)
         ((2000-01-01 00:00:01.000000-05:00)    32)
         ((2000-01-01 00:01:00.000000-05:00)    33)
         ((2000-01-01 01:00:00.000000-05:00)    34)
         ((2000-01-01 12:00:00.000000-05:00)    35)
         ((2000-01-01 13:42:23.622933-05:00)    36)
         ((2000-01-01 23:59:59.999999-05:00)    37)
         ((2000-01-02 00:00:00.000000-05:00)    38)
         ((2013-10-07 00:00:00.000000-04:00)    39)
         ((2013-10-07 00:00:00.000001-04:00)    40)
         ((2013-10-07 00:00:00.001000-04:00)    41)
         ((2013-10-07 00:00:01.000000-04:00)    42)
         ((2013-10-07 00:01:00.000000-04:00)    43)
         ((2013-10-07 01:00:00.000000-04:00)    44)
         ((2013-10-07 12:00:00.000000-04:00)    45)
         ((2013-10-07 13:42:23.622933-04:00)    46)
         ((2013-10-07 23:59:59.999999-04:00)    47)
         ((2013-10-08 00:00:00.000000-04:00)    48)
         ((2037-10-03 11:20:43.521111-04:00)    65)
         ((2076-09-15 07:19:49.513504-05:00)    66)
         ((2081-01-03 17:06:45.819349-05:00)    67)
         ((2175-12-01 10:47:07.694916-05:00)    68)
         ((2222-11-22 00:00:00.000000-05:00)    49)
         ((2222-11-22 00:00:00.000001-05:00)    50)
         ((2222-11-22 00:00:00.001000-05:00)    51)
         ((2222-11-22 00:00:01.000000-05:00)    52)
         ((2222-11-22 00:01:00.000000-05:00)    53)
         ((2222-11-22 01:00:00.000000-05:00)    54)
         ((2222-11-22 12:00:00.000000-05:00)    55)
         ((2222-11-22 13:42:23.622933-05:00)    56)
         ((2222-11-22 23:59:59.999999-05:00)    57)
         ((2222-11-23 00:00:00.000000-05:00)    58)))
       (bin_io
        "E\167\193`=\199I\001\194;\232\163\146\234v,\000\194<6\161\173\019\192\t\248\193=\127\229N\244\166\208\243\193>\144l\229n3\237\227\193?\000\000\000\140\241\012\219\193\000\252\255\255\139\241\012\219\193\001\158\239\255\139\241\012\219\193\002\000\000\192\139\241\012\219\193\003\000\000\000}\241\012\219\193\004\000\000\000\b\238\012\219\193\005\000\000\000\\\199\012\219\193\006\222!\024\\\193\012\219\193\007\004\000\000,\157\012\219\193\b\000\000\000,\157\012\219\193\t\000\000\000\000\000\148\209@\n\1901\004\000\000\148\209@\011\211Mb\016\000\148\209@\012\000\000\000\000@\148\209@\r\000\000\000\000\000\163\209@\014\000\000\000\000\000\024\213@\015\000\000\000\000\000\226\237@\016\234\151\136\247\249p\240@\017\145\243\254\255\255|\249@\018\000\000\000\000\000}\249@\019@\146\198W\015\242\202A@\000\000\000(\0286\204A\020\b\000\000(\0286\204A\021\197 \000(\0286\204A\022\000\000\128(\0286\204A\023\000\000\000F\0286\204A\024\000\000\0000#6\204A\025\000\000\000\136p6\204A\026E\188\207\135|6\204A\027\248\255\255\231\1966\204A\028\000\000\000\232\1966\204A\029\b\000\000\232\1966\204A\030\197 \000\232\1966\204A\031\000\000\128\232\1966\204A \000\000\000\006\1976\204A!\000\000\000\240\2036\204A\"\000\000\000H\0257\204A#E\188\207G%7\204A$\248\255\255\167m7\204A%\000\000\000\168m7\204A&\000\000\000p\140\148\212A'\004\000\000p\140\148\212A(b\016\000p\140\148\212A)\000\000@p\140\148\212A*\000\000\000\127\140\148\212A+\000\000\000\244\143\148\212A,\000\000\000\160\182\148\212A-\"\222\231\159\188\148\212A.\252\255\255\207\224\148\212A/\000\000\000\208\224\148\212A0\224Y\225\018\146\220\223AA\160n\176L\205\022\233AB\0288\186\014\195\025\234AC`\030\187\183\0215\248AD\000\000\000\181\189\186\253A1\001\000\000\181\189\186\253A2\025\004\000\181\189\186\253A3\000\000\016\181\189\186\253A4\000\000\192\184\189\186\253A5\000\000\000\150\190\186\253A6\000\000\000A\200\186\253A7\137\247\249\192\201\186\253A8\255\255\255\204\210\186\253A9\000\000\000\205\210\186\253A:"))
      |}]
  ;;

  (* [With_utc_sexp] *)

  let%expect_test "With_utc_sexp.V1" =
    test_stability
      (module struct
        include Time.Stable.With_utc_sexp.V1
        include For_time
      end);
    [%expect
      {|
      (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
      ((sexp (1912-06-23 05:00:00.000000Z))
       (bin_io "\000\000\000\140\241\012\219\193"))
      ((sexp (1912-06-23 05:00:00.000001Z))
       (bin_io "\252\255\255\139\241\012\219\193"))
      ((sexp (1912-06-23 05:00:00.001000Z))
       (bin_io "\158\239\255\139\241\012\219\193"))
      ((sexp (1912-06-23 05:00:01.000000Z))
       (bin_io "\000\000\192\139\241\012\219\193"))
      ((sexp (1912-06-23 05:01:00.000000Z))
       (bin_io "\000\000\000}\241\012\219\193"))
      ((sexp (1912-06-23 06:00:00.000000Z))
       (bin_io "\000\000\000\b\238\012\219\193"))
      ((sexp (1912-06-23 17:00:00.000000Z))
       (bin_io "\000\000\000\\\199\012\219\193"))
      ((sexp (1912-06-23 18:42:23.622933Z)) (bin_io "\222!\024\\\193\012\219\193"))
      ((sexp (1912-06-24 04:59:59.999999Z))
       (bin_io "\004\000\000,\157\012\219\193"))
      ((sexp (1912-06-24 05:00:00.000000Z))
       (bin_io "\000\000\000,\157\012\219\193"))
      ((sexp (1970-01-01 05:00:00.000000Z))
       (bin_io "\000\000\000\000\000\148\209@"))
      ((sexp (1970-01-01 05:00:00.000001Z)) (bin_io "\1901\004\000\000\148\209@"))
      ((sexp (1970-01-01 05:00:00.001000Z)) (bin_io "\211Mb\016\000\148\209@"))
      ((sexp (1970-01-01 05:00:01.000000Z)) (bin_io "\000\000\000\000@\148\209@"))
      ((sexp (1970-01-01 05:01:00.000000Z))
       (bin_io "\000\000\000\000\000\163\209@"))
      ((sexp (1970-01-01 06:00:00.000000Z))
       (bin_io "\000\000\000\000\000\024\213@"))
      ((sexp (1970-01-01 17:00:00.000000Z))
       (bin_io "\000\000\000\000\000\226\237@"))
      ((sexp (1970-01-01 18:42:23.622933Z)) (bin_io "\234\151\136\247\249p\240@"))
      ((sexp (1970-01-02 04:59:59.999999Z)) (bin_io "\145\243\254\255\255|\249@"))
      ((sexp (1970-01-02 05:00:00.000000Z)) (bin_io "\000\000\000\000\000}\249@"))
      ((sexp (1999-12-31 05:00:00.000000Z)) (bin_io "\000\000\000(\0286\204A"))
      ((sexp (1999-12-31 05:00:00.000001Z)) (bin_io "\b\000\000(\0286\204A"))
      ((sexp (1999-12-31 05:00:00.001000Z)) (bin_io "\197 \000(\0286\204A"))
      ((sexp (1999-12-31 05:00:01.000000Z)) (bin_io "\000\000\128(\0286\204A"))
      ((sexp (1999-12-31 05:01:00.000000Z)) (bin_io "\000\000\000F\0286\204A"))
      ((sexp (1999-12-31 06:00:00.000000Z)) (bin_io "\000\000\0000#6\204A"))
      ((sexp (1999-12-31 17:00:00.000000Z)) (bin_io "\000\000\000\136p6\204A"))
      ((sexp (1999-12-31 18:42:23.622933Z)) (bin_io "E\188\207\135|6\204A"))
      ((sexp (2000-01-01 04:59:59.999999Z)) (bin_io "\248\255\255\231\1966\204A"))
      ((sexp (2000-01-01 05:00:00.000000Z)) (bin_io "\000\000\000\232\1966\204A"))
      ((sexp (2000-01-01 05:00:00.000001Z)) (bin_io "\b\000\000\232\1966\204A"))
      ((sexp (2000-01-01 05:00:00.001000Z)) (bin_io "\197 \000\232\1966\204A"))
      ((sexp (2000-01-01 05:00:01.000000Z)) (bin_io "\000\000\128\232\1966\204A"))
      ((sexp (2000-01-01 05:01:00.000000Z)) (bin_io "\000\000\000\006\1976\204A"))
      ((sexp (2000-01-01 06:00:00.000000Z)) (bin_io "\000\000\000\240\2036\204A"))
      ((sexp (2000-01-01 17:00:00.000000Z)) (bin_io "\000\000\000H\0257\204A"))
      ((sexp (2000-01-01 18:42:23.622933Z)) (bin_io "E\188\207G%7\204A"))
      ((sexp (2000-01-02 04:59:59.999999Z)) (bin_io "\248\255\255\167m7\204A"))
      ((sexp (2000-01-02 05:00:00.000000Z)) (bin_io "\000\000\000\168m7\204A"))
      ((sexp (2013-10-07 04:00:00.000000Z)) (bin_io "\000\000\000p\140\148\212A"))
      ((sexp (2013-10-07 04:00:00.000001Z)) (bin_io "\004\000\000p\140\148\212A"))
      ((sexp (2013-10-07 04:00:00.001000Z)) (bin_io "b\016\000p\140\148\212A"))
      ((sexp (2013-10-07 04:00:01.000000Z)) (bin_io "\000\000@p\140\148\212A"))
      ((sexp (2013-10-07 04:01:00.000000Z))
       (bin_io "\000\000\000\127\140\148\212A"))
      ((sexp (2013-10-07 05:00:00.000000Z))
       (bin_io "\000\000\000\244\143\148\212A"))
      ((sexp (2013-10-07 16:00:00.000000Z))
       (bin_io "\000\000\000\160\182\148\212A"))
      ((sexp (2013-10-07 17:42:23.622933Z)) (bin_io "\"\222\231\159\188\148\212A"))
      ((sexp (2013-10-08 03:59:59.999999Z))
       (bin_io "\252\255\255\207\224\148\212A"))
      ((sexp (2013-10-08 04:00:00.000000Z))
       (bin_io "\000\000\000\208\224\148\212A"))
      ((sexp (2222-11-22 05:00:00.000000Z))
       (bin_io "\000\000\000\181\189\186\253A"))
      ((sexp (2222-11-22 05:00:00.000001Z))
       (bin_io "\001\000\000\181\189\186\253A"))
      ((sexp (2222-11-22 05:00:00.001000Z))
       (bin_io "\025\004\000\181\189\186\253A"))
      ((sexp (2222-11-22 05:00:01.000000Z))
       (bin_io "\000\000\016\181\189\186\253A"))
      ((sexp (2222-11-22 05:01:00.000000Z))
       (bin_io "\000\000\192\184\189\186\253A"))
      ((sexp (2222-11-22 06:00:00.000000Z))
       (bin_io "\000\000\000\150\190\186\253A"))
      ((sexp (2222-11-22 17:00:00.000000Z)) (bin_io "\000\000\000A\200\186\253A"))
      ((sexp (2222-11-22 18:42:23.622933Z))
       (bin_io "\137\247\249\192\201\186\253A"))
      ((sexp (2222-11-23 04:59:59.999999Z))
       (bin_io "\255\255\255\204\210\186\253A"))
      ((sexp (2222-11-23 05:00:00.000000Z))
       (bin_io "\000\000\000\205\210\186\253A"))
      ((sexp (1675-11-17 21:17:07.905443Z)) (bin_io "\167\193`=\199I\001\194"))
      ((sexp (1694-11-03 04:35:57.669968Z)) (bin_io "\232\163\146\234v,\000\194"))
      ((sexp (1765-07-10 06:18:45.148142Z)) (bin_io "6\161\173\019\192\t\248\193"))
      ((sexp (1801-06-12 18:58:03.068971Z))
       (bin_io "\127\229N\244\166\208\243\193"))
      ((sexp (1885-04-01 03:46:16.830498Z)) (bin_io "\144l\229n3\237\227\193"))
      ((sexp (1998-08-26 14:41:51.551338Z)) (bin_io "@\146\198W\015\242\202A"))
      ((sexp (2037-10-03 15:20:43.521111Z)) (bin_io "\224Y\225\018\146\220\223A"))
      ((sexp (2076-09-15 12:19:49.513504Z)) (bin_io "\160n\176L\205\022\233A"))
      ((sexp (2081-01-03 22:06:45.819349Z)) (bin_io "\0288\186\014\195\025\234A"))
      ((sexp (2175-12-01 15:47:07.694916Z)) (bin_io "`\030\187\183\0215\248A"))
      |}]
  ;;

  let%expect_test "With_utc_sexp.V1.Set" =
    test_stability
      (module struct
        include Time.Stable.With_utc_sexp.V1.Set
        include For_set
      end);
    [%expect
      {|
      (bin_shape_digest 4e7cbf6fe56bd628b963b7f8259e58bf)
      ((sexp ()) (bin_io "\000"))
      ((sexp ((1912-06-23 05:00:00.000000Z)))
       (bin_io "\001\000\000\000\140\241\012\219\193"))
      ((sexp ((1912-06-23 05:00:00.000001Z)))
       (bin_io "\001\252\255\255\139\241\012\219\193"))
      ((sexp ((1912-06-23 05:00:00.001000Z)))
       (bin_io "\001\158\239\255\139\241\012\219\193"))
      ((sexp ((1912-06-23 05:00:01.000000Z)))
       (bin_io "\001\000\000\192\139\241\012\219\193"))
      ((sexp ((1912-06-23 05:01:00.000000Z)))
       (bin_io "\001\000\000\000}\241\012\219\193"))
      ((sexp ((1912-06-23 06:00:00.000000Z)))
       (bin_io "\001\000\000\000\b\238\012\219\193"))
      ((sexp ((1912-06-23 17:00:00.000000Z)))
       (bin_io "\001\000\000\000\\\199\012\219\193"))
      ((sexp ((1912-06-23 18:42:23.622933Z)))
       (bin_io "\001\222!\024\\\193\012\219\193"))
      ((sexp ((1912-06-24 04:59:59.999999Z)))
       (bin_io "\001\004\000\000,\157\012\219\193"))
      ((sexp ((1912-06-24 05:00:00.000000Z)))
       (bin_io "\001\000\000\000,\157\012\219\193"))
      ((sexp ((1970-01-01 05:00:00.000000Z)))
       (bin_io "\001\000\000\000\000\000\148\209@"))
      ((sexp ((1970-01-01 05:00:00.000001Z)))
       (bin_io "\001\1901\004\000\000\148\209@"))
      ((sexp ((1970-01-01 05:00:00.001000Z)))
       (bin_io "\001\211Mb\016\000\148\209@"))
      ((sexp ((1970-01-01 05:00:01.000000Z)))
       (bin_io "\001\000\000\000\000@\148\209@"))
      ((sexp ((1970-01-01 05:01:00.000000Z)))
       (bin_io "\001\000\000\000\000\000\163\209@"))
      ((sexp ((1970-01-01 06:00:00.000000Z)))
       (bin_io "\001\000\000\000\000\000\024\213@"))
      ((sexp ((1970-01-01 17:00:00.000000Z)))
       (bin_io "\001\000\000\000\000\000\226\237@"))
      ((sexp ((1970-01-01 18:42:23.622933Z)))
       (bin_io "\001\234\151\136\247\249p\240@"))
      ((sexp ((1970-01-02 04:59:59.999999Z)))
       (bin_io "\001\145\243\254\255\255|\249@"))
      ((sexp ((1970-01-02 05:00:00.000000Z)))
       (bin_io "\001\000\000\000\000\000}\249@"))
      ((sexp ((1999-12-31 05:00:00.000000Z)))
       (bin_io "\001\000\000\000(\0286\204A"))
      ((sexp ((1999-12-31 05:00:00.000001Z))) (bin_io "\001\b\000\000(\0286\204A"))
      ((sexp ((1999-12-31 05:00:00.001000Z))) (bin_io "\001\197 \000(\0286\204A"))
      ((sexp ((1999-12-31 05:00:01.000000Z)))
       (bin_io "\001\000\000\128(\0286\204A"))
      ((sexp ((1999-12-31 05:01:00.000000Z)))
       (bin_io "\001\000\000\000F\0286\204A"))
      ((sexp ((1999-12-31 06:00:00.000000Z))) (bin_io "\001\000\000\0000#6\204A"))
      ((sexp ((1999-12-31 17:00:00.000000Z)))
       (bin_io "\001\000\000\000\136p6\204A"))
      ((sexp ((1999-12-31 18:42:23.622933Z))) (bin_io "\001E\188\207\135|6\204A"))
      ((sexp ((2000-01-01 04:59:59.999999Z)))
       (bin_io "\001\248\255\255\231\1966\204A"))
      ((sexp ((2000-01-01 05:00:00.000000Z)))
       (bin_io "\001\000\000\000\232\1966\204A"))
      ((sexp ((2000-01-01 05:00:00.000001Z)))
       (bin_io "\001\b\000\000\232\1966\204A"))
      ((sexp ((2000-01-01 05:00:00.001000Z)))
       (bin_io "\001\197 \000\232\1966\204A"))
      ((sexp ((2000-01-01 05:00:01.000000Z)))
       (bin_io "\001\000\000\128\232\1966\204A"))
      ((sexp ((2000-01-01 05:01:00.000000Z)))
       (bin_io "\001\000\000\000\006\1976\204A"))
      ((sexp ((2000-01-01 06:00:00.000000Z)))
       (bin_io "\001\000\000\000\240\2036\204A"))
      ((sexp ((2000-01-01 17:00:00.000000Z)))
       (bin_io "\001\000\000\000H\0257\204A"))
      ((sexp ((2000-01-01 18:42:23.622933Z))) (bin_io "\001E\188\207G%7\204A"))
      ((sexp ((2000-01-02 04:59:59.999999Z)))
       (bin_io "\001\248\255\255\167m7\204A"))
      ((sexp ((2000-01-02 05:00:00.000000Z)))
       (bin_io "\001\000\000\000\168m7\204A"))
      ((sexp ((2013-10-07 04:00:00.000000Z)))
       (bin_io "\001\000\000\000p\140\148\212A"))
      ((sexp ((2013-10-07 04:00:00.000001Z)))
       (bin_io "\001\004\000\000p\140\148\212A"))
      ((sexp ((2013-10-07 04:00:00.001000Z)))
       (bin_io "\001b\016\000p\140\148\212A"))
      ((sexp ((2013-10-07 04:00:01.000000Z)))
       (bin_io "\001\000\000@p\140\148\212A"))
      ((sexp ((2013-10-07 04:01:00.000000Z)))
       (bin_io "\001\000\000\000\127\140\148\212A"))
      ((sexp ((2013-10-07 05:00:00.000000Z)))
       (bin_io "\001\000\000\000\244\143\148\212A"))
      ((sexp ((2013-10-07 16:00:00.000000Z)))
       (bin_io "\001\000\000\000\160\182\148\212A"))
      ((sexp ((2013-10-07 17:42:23.622933Z)))
       (bin_io "\001\"\222\231\159\188\148\212A"))
      ((sexp ((2013-10-08 03:59:59.999999Z)))
       (bin_io "\001\252\255\255\207\224\148\212A"))
      ((sexp ((2013-10-08 04:00:00.000000Z)))
       (bin_io "\001\000\000\000\208\224\148\212A"))
      ((sexp ((2222-11-22 05:00:00.000000Z)))
       (bin_io "\001\000\000\000\181\189\186\253A"))
      ((sexp ((2222-11-22 05:00:00.000001Z)))
       (bin_io "\001\001\000\000\181\189\186\253A"))
      ((sexp ((2222-11-22 05:00:00.001000Z)))
       (bin_io "\001\025\004\000\181\189\186\253A"))
      ((sexp ((2222-11-22 05:00:01.000000Z)))
       (bin_io "\001\000\000\016\181\189\186\253A"))
      ((sexp ((2222-11-22 05:01:00.000000Z)))
       (bin_io "\001\000\000\192\184\189\186\253A"))
      ((sexp ((2222-11-22 06:00:00.000000Z)))
       (bin_io "\001\000\000\000\150\190\186\253A"))
      ((sexp ((2222-11-22 17:00:00.000000Z)))
       (bin_io "\001\000\000\000A\200\186\253A"))
      ((sexp ((2222-11-22 18:42:23.622933Z)))
       (bin_io "\001\137\247\249\192\201\186\253A"))
      ((sexp ((2222-11-23 04:59:59.999999Z)))
       (bin_io "\001\255\255\255\204\210\186\253A"))
      ((sexp ((2222-11-23 05:00:00.000000Z)))
       (bin_io "\001\000\000\000\205\210\186\253A"))
      ((sexp ((1675-11-17 21:17:07.905443Z)))
       (bin_io "\001\167\193`=\199I\001\194"))
      ((sexp ((1694-11-03 04:35:57.669968Z)))
       (bin_io "\001\232\163\146\234v,\000\194"))
      ((sexp ((1765-07-10 06:18:45.148142Z)))
       (bin_io "\0016\161\173\019\192\t\248\193"))
      ((sexp ((1801-06-12 18:58:03.068971Z)))
       (bin_io "\001\127\229N\244\166\208\243\193"))
      ((sexp ((1885-04-01 03:46:16.830498Z)))
       (bin_io "\001\144l\229n3\237\227\193"))
      ((sexp ((1998-08-26 14:41:51.551338Z)))
       (bin_io "\001@\146\198W\015\242\202A"))
      ((sexp ((2037-10-03 15:20:43.521111Z)))
       (bin_io "\001\224Y\225\018\146\220\223A"))
      ((sexp ((2076-09-15 12:19:49.513504Z)))
       (bin_io "\001\160n\176L\205\022\233A"))
      ((sexp ((2081-01-03 22:06:45.819349Z)))
       (bin_io "\001\0288\186\014\195\025\234A"))
      ((sexp ((2175-12-01 15:47:07.694916Z)))
       (bin_io "\001`\030\187\183\0215\248A"))
      ((sexp (
         (1675-11-17 21:17:07.905443Z)
         (1694-11-03 04:35:57.669968Z)
         (1765-07-10 06:18:45.148142Z)
         (1801-06-12 18:58:03.068971Z)
         (1885-04-01 03:46:16.830498Z)
         (1912-06-23 05:00:00.000000Z)
         (1912-06-23 05:00:00.000001Z)
         (1912-06-23 05:00:00.001000Z)
         (1912-06-23 05:00:01.000000Z)
         (1912-06-23 05:01:00.000000Z)
         (1912-06-23 06:00:00.000000Z)
         (1912-06-23 17:00:00.000000Z)
         (1912-06-23 18:42:23.622933Z)
         (1912-06-24 04:59:59.999999Z)
         (1912-06-24 05:00:00.000000Z)
         (1970-01-01 05:00:00.000000Z)
         (1970-01-01 05:00:00.000001Z)
         (1970-01-01 05:00:00.001000Z)
         (1970-01-01 05:00:01.000000Z)
         (1970-01-01 05:01:00.000000Z)
         (1970-01-01 06:00:00.000000Z)
         (1970-01-01 17:00:00.000000Z)
         (1970-01-01 18:42:23.622933Z)
         (1970-01-02 04:59:59.999999Z)
         (1970-01-02 05:00:00.000000Z)
         (1998-08-26 14:41:51.551338Z)
         (1999-12-31 05:00:00.000000Z)
         (1999-12-31 05:00:00.000001Z)
         (1999-12-31 05:00:00.001000Z)
         (1999-12-31 05:00:01.000000Z)
         (1999-12-31 05:01:00.000000Z)
         (1999-12-31 06:00:00.000000Z)
         (1999-12-31 17:00:00.000000Z)
         (1999-12-31 18:42:23.622933Z)
         (2000-01-01 04:59:59.999999Z)
         (2000-01-01 05:00:00.000000Z)
         (2000-01-01 05:00:00.000001Z)
         (2000-01-01 05:00:00.001000Z)
         (2000-01-01 05:00:01.000000Z)
         (2000-01-01 05:01:00.000000Z)
         (2000-01-01 06:00:00.000000Z)
         (2000-01-01 17:00:00.000000Z)
         (2000-01-01 18:42:23.622933Z)
         (2000-01-02 04:59:59.999999Z)
         (2000-01-02 05:00:00.000000Z)
         (2013-10-07 04:00:00.000000Z)
         (2013-10-07 04:00:00.000001Z)
         (2013-10-07 04:00:00.001000Z)
         (2013-10-07 04:00:01.000000Z)
         (2013-10-07 04:01:00.000000Z)
         (2013-10-07 05:00:00.000000Z)
         (2013-10-07 16:00:00.000000Z)
         (2013-10-07 17:42:23.622933Z)
         (2013-10-08 03:59:59.999999Z)
         (2013-10-08 04:00:00.000000Z)
         (2037-10-03 15:20:43.521111Z)
         (2076-09-15 12:19:49.513504Z)
         (2081-01-03 22:06:45.819349Z)
         (2175-12-01 15:47:07.694916Z)
         (2222-11-22 05:00:00.000000Z)
         (2222-11-22 05:00:00.000001Z)
         (2222-11-22 05:00:00.001000Z)
         (2222-11-22 05:00:01.000000Z)
         (2222-11-22 05:01:00.000000Z)
         (2222-11-22 06:00:00.000000Z)
         (2222-11-22 17:00:00.000000Z)
         (2222-11-22 18:42:23.622933Z)
         (2222-11-23 04:59:59.999999Z)
         (2222-11-23 05:00:00.000000Z)))
       (bin_io
        "E\167\193`=\199I\001\194\232\163\146\234v,\000\1946\161\173\019\192\t\248\193\127\229N\244\166\208\243\193\144l\229n3\237\227\193\000\000\000\140\241\012\219\193\252\255\255\139\241\012\219\193\158\239\255\139\241\012\219\193\000\000\192\139\241\012\219\193\000\000\000}\241\012\219\193\000\000\000\b\238\012\219\193\000\000\000\\\199\012\219\193\222!\024\\\193\012\219\193\004\000\000,\157\012\219\193\000\000\000,\157\012\219\193\000\000\000\000\000\148\209@\1901\004\000\000\148\209@\211Mb\016\000\148\209@\000\000\000\000@\148\209@\000\000\000\000\000\163\209@\000\000\000\000\000\024\213@\000\000\000\000\000\226\237@\234\151\136\247\249p\240@\145\243\254\255\255|\249@\000\000\000\000\000}\249@@\146\198W\015\242\202A\000\000\000(\0286\204A\b\000\000(\0286\204A\197 \000(\0286\204A\000\000\128(\0286\204A\000\000\000F\0286\204A\000\000\0000#6\204A\000\000\000\136p6\204AE\188\207\135|6\204A\248\255\255\231\1966\204A\000\000\000\232\1966\204A\b\000\000\232\1966\204A\197 \000\232\1966\204A\000\000\128\232\1966\204A\000\000\000\006\1976\204A\000\000\000\240\2036\204A\000\000\000H\0257\204AE\188\207G%7\204A\248\255\255\167m7\204A\000\000\000\168m7\204A\000\000\000p\140\148\212A\004\000\000p\140\148\212Ab\016\000p\140\148\212A\000\000@p\140\148\212A\000\000\000\127\140\148\212A\000\000\000\244\143\148\212A\000\000\000\160\182\148\212A\"\222\231\159\188\148\212A\252\255\255\207\224\148\212A\000\000\000\208\224\148\212A\224Y\225\018\146\220\223A\160n\176L\205\022\233A\0288\186\014\195\025\234A`\030\187\183\0215\248A\000\000\000\181\189\186\253A\001\000\000\181\189\186\253A\025\004\000\181\189\186\253A\000\000\016\181\189\186\253A\000\000\192\184\189\186\253A\000\000\000\150\190\186\253A\000\000\000A\200\186\253A\137\247\249\192\201\186\253A\255\255\255\204\210\186\253A\000\000\000\205\210\186\253A"))
      |}]
  ;;

  let%expect_test "With_utc_sexp.V1.Map" =
    test_stability
      (module struct
        include Specialize_to_int (Time.Stable.With_utc_sexp.V1.Map)
        include For_map
      end);
    [%expect
      {|
      (bin_shape_digest 31404094f08cdbe1f9fca07a1a1e5303)
      ((sexp ()) (bin_io "\000"))
      ((sexp (((1912-06-23 05:00:00.000000Z) 0)))
       (bin_io "\001\000\000\000\140\241\012\219\193\000"))
      ((sexp (((1912-06-23 05:00:00.000001Z) 1)))
       (bin_io "\001\252\255\255\139\241\012\219\193\001"))
      ((sexp (((1912-06-23 05:00:00.001000Z) 2)))
       (bin_io "\001\158\239\255\139\241\012\219\193\002"))
      ((sexp (((1912-06-23 05:00:01.000000Z) 3)))
       (bin_io "\001\000\000\192\139\241\012\219\193\003"))
      ((sexp (((1912-06-23 05:01:00.000000Z) 4)))
       (bin_io "\001\000\000\000}\241\012\219\193\004"))
      ((sexp (((1912-06-23 06:00:00.000000Z) 5)))
       (bin_io "\001\000\000\000\b\238\012\219\193\005"))
      ((sexp (((1912-06-23 17:00:00.000000Z) 6)))
       (bin_io "\001\000\000\000\\\199\012\219\193\006"))
      ((sexp (((1912-06-23 18:42:23.622933Z) 7)))
       (bin_io "\001\222!\024\\\193\012\219\193\007"))
      ((sexp (((1912-06-24 04:59:59.999999Z) 8)))
       (bin_io "\001\004\000\000,\157\012\219\193\b"))
      ((sexp (((1912-06-24 05:00:00.000000Z) 9)))
       (bin_io "\001\000\000\000,\157\012\219\193\t"))
      ((sexp (((1970-01-01 05:00:00.000000Z) 10)))
       (bin_io "\001\000\000\000\000\000\148\209@\n"))
      ((sexp (((1970-01-01 05:00:00.000001Z) 11)))
       (bin_io "\001\1901\004\000\000\148\209@\011"))
      ((sexp (((1970-01-01 05:00:00.001000Z) 12)))
       (bin_io "\001\211Mb\016\000\148\209@\012"))
      ((sexp (((1970-01-01 05:00:01.000000Z) 13)))
       (bin_io "\001\000\000\000\000@\148\209@\r"))
      ((sexp (((1970-01-01 05:01:00.000000Z) 14)))
       (bin_io "\001\000\000\000\000\000\163\209@\014"))
      ((sexp (((1970-01-01 06:00:00.000000Z) 15)))
       (bin_io "\001\000\000\000\000\000\024\213@\015"))
      ((sexp (((1970-01-01 17:00:00.000000Z) 16)))
       (bin_io "\001\000\000\000\000\000\226\237@\016"))
      ((sexp (((1970-01-01 18:42:23.622933Z) 17)))
       (bin_io "\001\234\151\136\247\249p\240@\017"))
      ((sexp (((1970-01-02 04:59:59.999999Z) 18)))
       (bin_io "\001\145\243\254\255\255|\249@\018"))
      ((sexp (((1970-01-02 05:00:00.000000Z) 19)))
       (bin_io "\001\000\000\000\000\000}\249@\019"))
      ((sexp (((1999-12-31 05:00:00.000000Z) 20)))
       (bin_io "\001\000\000\000(\0286\204A\020"))
      ((sexp (((1999-12-31 05:00:00.000001Z) 21)))
       (bin_io "\001\b\000\000(\0286\204A\021"))
      ((sexp (((1999-12-31 05:00:00.001000Z) 22)))
       (bin_io "\001\197 \000(\0286\204A\022"))
      ((sexp (((1999-12-31 05:00:01.000000Z) 23)))
       (bin_io "\001\000\000\128(\0286\204A\023"))
      ((sexp (((1999-12-31 05:01:00.000000Z) 24)))
       (bin_io "\001\000\000\000F\0286\204A\024"))
      ((sexp (((1999-12-31 06:00:00.000000Z) 25)))
       (bin_io "\001\000\000\0000#6\204A\025"))
      ((sexp (((1999-12-31 17:00:00.000000Z) 26)))
       (bin_io "\001\000\000\000\136p6\204A\026"))
      ((sexp (((1999-12-31 18:42:23.622933Z) 27)))
       (bin_io "\001E\188\207\135|6\204A\027"))
      ((sexp (((2000-01-01 04:59:59.999999Z) 28)))
       (bin_io "\001\248\255\255\231\1966\204A\028"))
      ((sexp (((2000-01-01 05:00:00.000000Z) 29)))
       (bin_io "\001\000\000\000\232\1966\204A\029"))
      ((sexp (((2000-01-01 05:00:00.000001Z) 30)))
       (bin_io "\001\b\000\000\232\1966\204A\030"))
      ((sexp (((2000-01-01 05:00:00.001000Z) 31)))
       (bin_io "\001\197 \000\232\1966\204A\031"))
      ((sexp (((2000-01-01 05:00:01.000000Z) 32)))
       (bin_io "\001\000\000\128\232\1966\204A "))
      ((sexp (((2000-01-01 05:01:00.000000Z) 33)))
       (bin_io "\001\000\000\000\006\1976\204A!"))
      ((sexp (((2000-01-01 06:00:00.000000Z) 34)))
       (bin_io "\001\000\000\000\240\2036\204A\""))
      ((sexp (((2000-01-01 17:00:00.000000Z) 35)))
       (bin_io "\001\000\000\000H\0257\204A#"))
      ((sexp (((2000-01-01 18:42:23.622933Z) 36)))
       (bin_io "\001E\188\207G%7\204A$"))
      ((sexp (((2000-01-02 04:59:59.999999Z) 37)))
       (bin_io "\001\248\255\255\167m7\204A%"))
      ((sexp (((2000-01-02 05:00:00.000000Z) 38)))
       (bin_io "\001\000\000\000\168m7\204A&"))
      ((sexp (((2013-10-07 04:00:00.000000Z) 39)))
       (bin_io "\001\000\000\000p\140\148\212A'"))
      ((sexp (((2013-10-07 04:00:00.000001Z) 40)))
       (bin_io "\001\004\000\000p\140\148\212A("))
      ((sexp (((2013-10-07 04:00:00.001000Z) 41)))
       (bin_io "\001b\016\000p\140\148\212A)"))
      ((sexp (((2013-10-07 04:00:01.000000Z) 42)))
       (bin_io "\001\000\000@p\140\148\212A*"))
      ((sexp (((2013-10-07 04:01:00.000000Z) 43)))
       (bin_io "\001\000\000\000\127\140\148\212A+"))
      ((sexp (((2013-10-07 05:00:00.000000Z) 44)))
       (bin_io "\001\000\000\000\244\143\148\212A,"))
      ((sexp (((2013-10-07 16:00:00.000000Z) 45)))
       (bin_io "\001\000\000\000\160\182\148\212A-"))
      ((sexp (((2013-10-07 17:42:23.622933Z) 46)))
       (bin_io "\001\"\222\231\159\188\148\212A."))
      ((sexp (((2013-10-08 03:59:59.999999Z) 47)))
       (bin_io "\001\252\255\255\207\224\148\212A/"))
      ((sexp (((2013-10-08 04:00:00.000000Z) 48)))
       (bin_io "\001\000\000\000\208\224\148\212A0"))
      ((sexp (((2222-11-22 05:00:00.000000Z) 49)))
       (bin_io "\001\000\000\000\181\189\186\253A1"))
      ((sexp (((2222-11-22 05:00:00.000001Z) 50)))
       (bin_io "\001\001\000\000\181\189\186\253A2"))
      ((sexp (((2222-11-22 05:00:00.001000Z) 51)))
       (bin_io "\001\025\004\000\181\189\186\253A3"))
      ((sexp (((2222-11-22 05:00:01.000000Z) 52)))
       (bin_io "\001\000\000\016\181\189\186\253A4"))
      ((sexp (((2222-11-22 05:01:00.000000Z) 53)))
       (bin_io "\001\000\000\192\184\189\186\253A5"))
      ((sexp (((2222-11-22 06:00:00.000000Z) 54)))
       (bin_io "\001\000\000\000\150\190\186\253A6"))
      ((sexp (((2222-11-22 17:00:00.000000Z) 55)))
       (bin_io "\001\000\000\000A\200\186\253A7"))
      ((sexp (((2222-11-22 18:42:23.622933Z) 56)))
       (bin_io "\001\137\247\249\192\201\186\253A8"))
      ((sexp (((2222-11-23 04:59:59.999999Z) 57)))
       (bin_io "\001\255\255\255\204\210\186\253A9"))
      ((sexp (((2222-11-23 05:00:00.000000Z) 58)))
       (bin_io "\001\000\000\000\205\210\186\253A:"))
      ((sexp (((1675-11-17 21:17:07.905443Z) 59)))
       (bin_io "\001\167\193`=\199I\001\194;"))
      ((sexp (((1694-11-03 04:35:57.669968Z) 60)))
       (bin_io "\001\232\163\146\234v,\000\194<"))
      ((sexp (((1765-07-10 06:18:45.148142Z) 61)))
       (bin_io "\0016\161\173\019\192\t\248\193="))
      ((sexp (((1801-06-12 18:58:03.068971Z) 62)))
       (bin_io "\001\127\229N\244\166\208\243\193>"))
      ((sexp (((1885-04-01 03:46:16.830498Z) 63)))
       (bin_io "\001\144l\229n3\237\227\193?"))
      ((sexp (((1998-08-26 14:41:51.551338Z) 64)))
       (bin_io "\001@\146\198W\015\242\202A@"))
      ((sexp (((2037-10-03 15:20:43.521111Z) 65)))
       (bin_io "\001\224Y\225\018\146\220\223AA"))
      ((sexp (((2076-09-15 12:19:49.513504Z) 66)))
       (bin_io "\001\160n\176L\205\022\233AB"))
      ((sexp (((2081-01-03 22:06:45.819349Z) 67)))
       (bin_io "\001\0288\186\014\195\025\234AC"))
      ((sexp (((2175-12-01 15:47:07.694916Z) 68)))
       (bin_io "\001`\030\187\183\0215\248AD"))
      ((sexp (
         ((1675-11-17 21:17:07.905443Z) 59)
         ((1694-11-03 04:35:57.669968Z) 60)
         ((1765-07-10 06:18:45.148142Z) 61)
         ((1801-06-12 18:58:03.068971Z) 62)
         ((1885-04-01 03:46:16.830498Z) 63)
         ((1912-06-23 05:00:00.000000Z) 0)
         ((1912-06-23 05:00:00.000001Z) 1)
         ((1912-06-23 05:00:00.001000Z) 2)
         ((1912-06-23 05:00:01.000000Z) 3)
         ((1912-06-23 05:01:00.000000Z) 4)
         ((1912-06-23 06:00:00.000000Z) 5)
         ((1912-06-23 17:00:00.000000Z) 6)
         ((1912-06-23 18:42:23.622933Z) 7)
         ((1912-06-24 04:59:59.999999Z) 8)
         ((1912-06-24 05:00:00.000000Z) 9)
         ((1970-01-01 05:00:00.000000Z) 10)
         ((1970-01-01 05:00:00.000001Z) 11)
         ((1970-01-01 05:00:00.001000Z) 12)
         ((1970-01-01 05:00:01.000000Z) 13)
         ((1970-01-01 05:01:00.000000Z) 14)
         ((1970-01-01 06:00:00.000000Z) 15)
         ((1970-01-01 17:00:00.000000Z) 16)
         ((1970-01-01 18:42:23.622933Z) 17)
         ((1970-01-02 04:59:59.999999Z) 18)
         ((1970-01-02 05:00:00.000000Z) 19)
         ((1998-08-26 14:41:51.551338Z) 64)
         ((1999-12-31 05:00:00.000000Z) 20)
         ((1999-12-31 05:00:00.000001Z) 21)
         ((1999-12-31 05:00:00.001000Z) 22)
         ((1999-12-31 05:00:01.000000Z) 23)
         ((1999-12-31 05:01:00.000000Z) 24)
         ((1999-12-31 06:00:00.000000Z) 25)
         ((1999-12-31 17:00:00.000000Z) 26)
         ((1999-12-31 18:42:23.622933Z) 27)
         ((2000-01-01 04:59:59.999999Z) 28)
         ((2000-01-01 05:00:00.000000Z) 29)
         ((2000-01-01 05:00:00.000001Z) 30)
         ((2000-01-01 05:00:00.001000Z) 31)
         ((2000-01-01 05:00:01.000000Z) 32)
         ((2000-01-01 05:01:00.000000Z) 33)
         ((2000-01-01 06:00:00.000000Z) 34)
         ((2000-01-01 17:00:00.000000Z) 35)
         ((2000-01-01 18:42:23.622933Z) 36)
         ((2000-01-02 04:59:59.999999Z) 37)
         ((2000-01-02 05:00:00.000000Z) 38)
         ((2013-10-07 04:00:00.000000Z) 39)
         ((2013-10-07 04:00:00.000001Z) 40)
         ((2013-10-07 04:00:00.001000Z) 41)
         ((2013-10-07 04:00:01.000000Z) 42)
         ((2013-10-07 04:01:00.000000Z) 43)
         ((2013-10-07 05:00:00.000000Z) 44)
         ((2013-10-07 16:00:00.000000Z) 45)
         ((2013-10-07 17:42:23.622933Z) 46)
         ((2013-10-08 03:59:59.999999Z) 47)
         ((2013-10-08 04:00:00.000000Z) 48)
         ((2037-10-03 15:20:43.521111Z) 65)
         ((2076-09-15 12:19:49.513504Z) 66)
         ((2081-01-03 22:06:45.819349Z) 67)
         ((2175-12-01 15:47:07.694916Z) 68)
         ((2222-11-22 05:00:00.000000Z) 49)
         ((2222-11-22 05:00:00.000001Z) 50)
         ((2222-11-22 05:00:00.001000Z) 51)
         ((2222-11-22 05:00:01.000000Z) 52)
         ((2222-11-22 05:01:00.000000Z) 53)
         ((2222-11-22 06:00:00.000000Z) 54)
         ((2222-11-22 17:00:00.000000Z) 55)
         ((2222-11-22 18:42:23.622933Z) 56)
         ((2222-11-23 04:59:59.999999Z) 57)
         ((2222-11-23 05:00:00.000000Z) 58)))
       (bin_io
        "E\167\193`=\199I\001\194;\232\163\146\234v,\000\194<6\161\173\019\192\t\248\193=\127\229N\244\166\208\243\193>\144l\229n3\237\227\193?\000\000\000\140\241\012\219\193\000\252\255\255\139\241\012\219\193\001\158\239\255\139\241\012\219\193\002\000\000\192\139\241\012\219\193\003\000\000\000}\241\012\219\193\004\000\000\000\b\238\012\219\193\005\000\000\000\\\199\012\219\193\006\222!\024\\\193\012\219\193\007\004\000\000,\157\012\219\193\b\000\000\000,\157\012\219\193\t\000\000\000\000\000\148\209@\n\1901\004\000\000\148\209@\011\211Mb\016\000\148\209@\012\000\000\000\000@\148\209@\r\000\000\000\000\000\163\209@\014\000\000\000\000\000\024\213@\015\000\000\000\000\000\226\237@\016\234\151\136\247\249p\240@\017\145\243\254\255\255|\249@\018\000\000\000\000\000}\249@\019@\146\198W\015\242\202A@\000\000\000(\0286\204A\020\b\000\000(\0286\204A\021\197 \000(\0286\204A\022\000\000\128(\0286\204A\023\000\000\000F\0286\204A\024\000\000\0000#6\204A\025\000\000\000\136p6\204A\026E\188\207\135|6\204A\027\248\255\255\231\1966\204A\028\000\000\000\232\1966\204A\029\b\000\000\232\1966\204A\030\197 \000\232\1966\204A\031\000\000\128\232\1966\204A \000\000\000\006\1976\204A!\000\000\000\240\2036\204A\"\000\000\000H\0257\204A#E\188\207G%7\204A$\248\255\255\167m7\204A%\000\000\000\168m7\204A&\000\000\000p\140\148\212A'\004\000\000p\140\148\212A(b\016\000p\140\148\212A)\000\000@p\140\148\212A*\000\000\000\127\140\148\212A+\000\000\000\244\143\148\212A,\000\000\000\160\182\148\212A-\"\222\231\159\188\148\212A.\252\255\255\207\224\148\212A/\000\000\000\208\224\148\212A0\224Y\225\018\146\220\223AA\160n\176L\205\022\233AB\0288\186\014\195\025\234AC`\030\187\183\0215\248AD\000\000\000\181\189\186\253A1\001\000\000\181\189\186\253A2\025\004\000\181\189\186\253A3\000\000\016\181\189\186\253A4\000\000\192\184\189\186\253A5\000\000\000\150\190\186\253A6\000\000\000A\200\186\253A7\137\247\249\192\201\186\253A8\255\255\255\204\210\186\253A9\000\000\000\205\210\186\253A:"))
      |}]
  ;;

  let%expect_test "With_utc_sexp.V2" =
    test_stability
      (module struct
        include Time.Stable.With_utc_sexp.V2
        include For_time
      end);
    [%expect
      {|
      (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
      ((sexp (1912-06-23 05:00:00.000000Z))
       (bin_io "\000\000\000\140\241\012\219\193"))
      ((sexp (1912-06-23 05:00:00.000001Z))
       (bin_io "\252\255\255\139\241\012\219\193"))
      ((sexp (1912-06-23 05:00:00.001000Z))
       (bin_io "\158\239\255\139\241\012\219\193"))
      ((sexp (1912-06-23 05:00:01.000000Z))
       (bin_io "\000\000\192\139\241\012\219\193"))
      ((sexp (1912-06-23 05:01:00.000000Z))
       (bin_io "\000\000\000}\241\012\219\193"))
      ((sexp (1912-06-23 06:00:00.000000Z))
       (bin_io "\000\000\000\b\238\012\219\193"))
      ((sexp (1912-06-23 17:00:00.000000Z))
       (bin_io "\000\000\000\\\199\012\219\193"))
      ((sexp (1912-06-23 18:42:23.622933Z)) (bin_io "\222!\024\\\193\012\219\193"))
      ((sexp (1912-06-24 04:59:59.999999Z))
       (bin_io "\004\000\000,\157\012\219\193"))
      ((sexp (1912-06-24 05:00:00.000000Z))
       (bin_io "\000\000\000,\157\012\219\193"))
      ((sexp (1970-01-01 05:00:00.000000Z))
       (bin_io "\000\000\000\000\000\148\209@"))
      ((sexp (1970-01-01 05:00:00.000001Z)) (bin_io "\1901\004\000\000\148\209@"))
      ((sexp (1970-01-01 05:00:00.001000Z)) (bin_io "\211Mb\016\000\148\209@"))
      ((sexp (1970-01-01 05:00:01.000000Z)) (bin_io "\000\000\000\000@\148\209@"))
      ((sexp (1970-01-01 05:01:00.000000Z))
       (bin_io "\000\000\000\000\000\163\209@"))
      ((sexp (1970-01-01 06:00:00.000000Z))
       (bin_io "\000\000\000\000\000\024\213@"))
      ((sexp (1970-01-01 17:00:00.000000Z))
       (bin_io "\000\000\000\000\000\226\237@"))
      ((sexp (1970-01-01 18:42:23.622933Z)) (bin_io "\234\151\136\247\249p\240@"))
      ((sexp (1970-01-02 04:59:59.999999Z)) (bin_io "\145\243\254\255\255|\249@"))
      ((sexp (1970-01-02 05:00:00.000000Z)) (bin_io "\000\000\000\000\000}\249@"))
      ((sexp (1999-12-31 05:00:00.000000Z)) (bin_io "\000\000\000(\0286\204A"))
      ((sexp (1999-12-31 05:00:00.000001Z)) (bin_io "\b\000\000(\0286\204A"))
      ((sexp (1999-12-31 05:00:00.001000Z)) (bin_io "\197 \000(\0286\204A"))
      ((sexp (1999-12-31 05:00:01.000000Z)) (bin_io "\000\000\128(\0286\204A"))
      ((sexp (1999-12-31 05:01:00.000000Z)) (bin_io "\000\000\000F\0286\204A"))
      ((sexp (1999-12-31 06:00:00.000000Z)) (bin_io "\000\000\0000#6\204A"))
      ((sexp (1999-12-31 17:00:00.000000Z)) (bin_io "\000\000\000\136p6\204A"))
      ((sexp (1999-12-31 18:42:23.622933Z)) (bin_io "E\188\207\135|6\204A"))
      ((sexp (2000-01-01 04:59:59.999999Z)) (bin_io "\248\255\255\231\1966\204A"))
      ((sexp (2000-01-01 05:00:00.000000Z)) (bin_io "\000\000\000\232\1966\204A"))
      ((sexp (2000-01-01 05:00:00.000001Z)) (bin_io "\b\000\000\232\1966\204A"))
      ((sexp (2000-01-01 05:00:00.001000Z)) (bin_io "\197 \000\232\1966\204A"))
      ((sexp (2000-01-01 05:00:01.000000Z)) (bin_io "\000\000\128\232\1966\204A"))
      ((sexp (2000-01-01 05:01:00.000000Z)) (bin_io "\000\000\000\006\1976\204A"))
      ((sexp (2000-01-01 06:00:00.000000Z)) (bin_io "\000\000\000\240\2036\204A"))
      ((sexp (2000-01-01 17:00:00.000000Z)) (bin_io "\000\000\000H\0257\204A"))
      ((sexp (2000-01-01 18:42:23.622933Z)) (bin_io "E\188\207G%7\204A"))
      ((sexp (2000-01-02 04:59:59.999999Z)) (bin_io "\248\255\255\167m7\204A"))
      ((sexp (2000-01-02 05:00:00.000000Z)) (bin_io "\000\000\000\168m7\204A"))
      ((sexp (2013-10-07 04:00:00.000000Z)) (bin_io "\000\000\000p\140\148\212A"))
      ((sexp (2013-10-07 04:00:00.000001Z)) (bin_io "\004\000\000p\140\148\212A"))
      ((sexp (2013-10-07 04:00:00.001000Z)) (bin_io "b\016\000p\140\148\212A"))
      ((sexp (2013-10-07 04:00:01.000000Z)) (bin_io "\000\000@p\140\148\212A"))
      ((sexp (2013-10-07 04:01:00.000000Z))
       (bin_io "\000\000\000\127\140\148\212A"))
      ((sexp (2013-10-07 05:00:00.000000Z))
       (bin_io "\000\000\000\244\143\148\212A"))
      ((sexp (2013-10-07 16:00:00.000000Z))
       (bin_io "\000\000\000\160\182\148\212A"))
      ((sexp (2013-10-07 17:42:23.622933Z)) (bin_io "\"\222\231\159\188\148\212A"))
      ((sexp (2013-10-08 03:59:59.999999Z))
       (bin_io "\252\255\255\207\224\148\212A"))
      ((sexp (2013-10-08 04:00:00.000000Z))
       (bin_io "\000\000\000\208\224\148\212A"))
      ((sexp (2222-11-22 05:00:00.000000Z))
       (bin_io "\000\000\000\181\189\186\253A"))
      ((sexp (2222-11-22 05:00:00.000001Z))
       (bin_io "\001\000\000\181\189\186\253A"))
      ((sexp (2222-11-22 05:00:00.001000Z))
       (bin_io "\025\004\000\181\189\186\253A"))
      ((sexp (2222-11-22 05:00:01.000000Z))
       (bin_io "\000\000\016\181\189\186\253A"))
      ((sexp (2222-11-22 05:01:00.000000Z))
       (bin_io "\000\000\192\184\189\186\253A"))
      ((sexp (2222-11-22 06:00:00.000000Z))
       (bin_io "\000\000\000\150\190\186\253A"))
      ((sexp (2222-11-22 17:00:00.000000Z)) (bin_io "\000\000\000A\200\186\253A"))
      ((sexp (2222-11-22 18:42:23.622933Z))
       (bin_io "\137\247\249\192\201\186\253A"))
      ((sexp (2222-11-23 04:59:59.999999Z))
       (bin_io "\255\255\255\204\210\186\253A"))
      ((sexp (2222-11-23 05:00:00.000000Z))
       (bin_io "\000\000\000\205\210\186\253A"))
      ((sexp (1675-11-17 21:17:07.905443Z)) (bin_io "\167\193`=\199I\001\194"))
      ((sexp (1694-11-03 04:35:57.669968Z)) (bin_io "\232\163\146\234v,\000\194"))
      ((sexp (1765-07-10 06:18:45.148142Z)) (bin_io "6\161\173\019\192\t\248\193"))
      ((sexp (1801-06-12 18:58:03.068971Z))
       (bin_io "\127\229N\244\166\208\243\193"))
      ((sexp (1885-04-01 03:46:16.830498Z)) (bin_io "\144l\229n3\237\227\193"))
      ((sexp (1998-08-26 14:41:51.551338Z)) (bin_io "@\146\198W\015\242\202A"))
      ((sexp (2037-10-03 15:20:43.521111Z)) (bin_io "\224Y\225\018\146\220\223A"))
      ((sexp (2076-09-15 12:19:49.513504Z)) (bin_io "\160n\176L\205\022\233A"))
      ((sexp (2081-01-03 22:06:45.819349Z)) (bin_io "\0288\186\014\195\025\234A"))
      ((sexp (2175-12-01 15:47:07.694916Z)) (bin_io "`\030\187\183\0215\248A"))
      |}]
  ;;

  let%expect_test "With_utc_sexp.V2.Set" =
    test_stability
      (module struct
        include Time.Stable.With_utc_sexp.V2.Set
        include For_set
      end);
    [%expect
      {|
      (bin_shape_digest 4e7cbf6fe56bd628b963b7f8259e58bf)
      ((sexp ()) (bin_io "\000"))
      ((sexp ((1912-06-23 05:00:00.000000Z)))
       (bin_io "\001\000\000\000\140\241\012\219\193"))
      ((sexp ((1912-06-23 05:00:00.000001Z)))
       (bin_io "\001\252\255\255\139\241\012\219\193"))
      ((sexp ((1912-06-23 05:00:00.001000Z)))
       (bin_io "\001\158\239\255\139\241\012\219\193"))
      ((sexp ((1912-06-23 05:00:01.000000Z)))
       (bin_io "\001\000\000\192\139\241\012\219\193"))
      ((sexp ((1912-06-23 05:01:00.000000Z)))
       (bin_io "\001\000\000\000}\241\012\219\193"))
      ((sexp ((1912-06-23 06:00:00.000000Z)))
       (bin_io "\001\000\000\000\b\238\012\219\193"))
      ((sexp ((1912-06-23 17:00:00.000000Z)))
       (bin_io "\001\000\000\000\\\199\012\219\193"))
      ((sexp ((1912-06-23 18:42:23.622933Z)))
       (bin_io "\001\222!\024\\\193\012\219\193"))
      ((sexp ((1912-06-24 04:59:59.999999Z)))
       (bin_io "\001\004\000\000,\157\012\219\193"))
      ((sexp ((1912-06-24 05:00:00.000000Z)))
       (bin_io "\001\000\000\000,\157\012\219\193"))
      ((sexp ((1970-01-01 05:00:00.000000Z)))
       (bin_io "\001\000\000\000\000\000\148\209@"))
      ((sexp ((1970-01-01 05:00:00.000001Z)))
       (bin_io "\001\1901\004\000\000\148\209@"))
      ((sexp ((1970-01-01 05:00:00.001000Z)))
       (bin_io "\001\211Mb\016\000\148\209@"))
      ((sexp ((1970-01-01 05:00:01.000000Z)))
       (bin_io "\001\000\000\000\000@\148\209@"))
      ((sexp ((1970-01-01 05:01:00.000000Z)))
       (bin_io "\001\000\000\000\000\000\163\209@"))
      ((sexp ((1970-01-01 06:00:00.000000Z)))
       (bin_io "\001\000\000\000\000\000\024\213@"))
      ((sexp ((1970-01-01 17:00:00.000000Z)))
       (bin_io "\001\000\000\000\000\000\226\237@"))
      ((sexp ((1970-01-01 18:42:23.622933Z)))
       (bin_io "\001\234\151\136\247\249p\240@"))
      ((sexp ((1970-01-02 04:59:59.999999Z)))
       (bin_io "\001\145\243\254\255\255|\249@"))
      ((sexp ((1970-01-02 05:00:00.000000Z)))
       (bin_io "\001\000\000\000\000\000}\249@"))
      ((sexp ((1999-12-31 05:00:00.000000Z)))
       (bin_io "\001\000\000\000(\0286\204A"))
      ((sexp ((1999-12-31 05:00:00.000001Z))) (bin_io "\001\b\000\000(\0286\204A"))
      ((sexp ((1999-12-31 05:00:00.001000Z))) (bin_io "\001\197 \000(\0286\204A"))
      ((sexp ((1999-12-31 05:00:01.000000Z)))
       (bin_io "\001\000\000\128(\0286\204A"))
      ((sexp ((1999-12-31 05:01:00.000000Z)))
       (bin_io "\001\000\000\000F\0286\204A"))
      ((sexp ((1999-12-31 06:00:00.000000Z))) (bin_io "\001\000\000\0000#6\204A"))
      ((sexp ((1999-12-31 17:00:00.000000Z)))
       (bin_io "\001\000\000\000\136p6\204A"))
      ((sexp ((1999-12-31 18:42:23.622933Z))) (bin_io "\001E\188\207\135|6\204A"))
      ((sexp ((2000-01-01 04:59:59.999999Z)))
       (bin_io "\001\248\255\255\231\1966\204A"))
      ((sexp ((2000-01-01 05:00:00.000000Z)))
       (bin_io "\001\000\000\000\232\1966\204A"))
      ((sexp ((2000-01-01 05:00:00.000001Z)))
       (bin_io "\001\b\000\000\232\1966\204A"))
      ((sexp ((2000-01-01 05:00:00.001000Z)))
       (bin_io "\001\197 \000\232\1966\204A"))
      ((sexp ((2000-01-01 05:00:01.000000Z)))
       (bin_io "\001\000\000\128\232\1966\204A"))
      ((sexp ((2000-01-01 05:01:00.000000Z)))
       (bin_io "\001\000\000\000\006\1976\204A"))
      ((sexp ((2000-01-01 06:00:00.000000Z)))
       (bin_io "\001\000\000\000\240\2036\204A"))
      ((sexp ((2000-01-01 17:00:00.000000Z)))
       (bin_io "\001\000\000\000H\0257\204A"))
      ((sexp ((2000-01-01 18:42:23.622933Z))) (bin_io "\001E\188\207G%7\204A"))
      ((sexp ((2000-01-02 04:59:59.999999Z)))
       (bin_io "\001\248\255\255\167m7\204A"))
      ((sexp ((2000-01-02 05:00:00.000000Z)))
       (bin_io "\001\000\000\000\168m7\204A"))
      ((sexp ((2013-10-07 04:00:00.000000Z)))
       (bin_io "\001\000\000\000p\140\148\212A"))
      ((sexp ((2013-10-07 04:00:00.000001Z)))
       (bin_io "\001\004\000\000p\140\148\212A"))
      ((sexp ((2013-10-07 04:00:00.001000Z)))
       (bin_io "\001b\016\000p\140\148\212A"))
      ((sexp ((2013-10-07 04:00:01.000000Z)))
       (bin_io "\001\000\000@p\140\148\212A"))
      ((sexp ((2013-10-07 04:01:00.000000Z)))
       (bin_io "\001\000\000\000\127\140\148\212A"))
      ((sexp ((2013-10-07 05:00:00.000000Z)))
       (bin_io "\001\000\000\000\244\143\148\212A"))
      ((sexp ((2013-10-07 16:00:00.000000Z)))
       (bin_io "\001\000\000\000\160\182\148\212A"))
      ((sexp ((2013-10-07 17:42:23.622933Z)))
       (bin_io "\001\"\222\231\159\188\148\212A"))
      ((sexp ((2013-10-08 03:59:59.999999Z)))
       (bin_io "\001\252\255\255\207\224\148\212A"))
      ((sexp ((2013-10-08 04:00:00.000000Z)))
       (bin_io "\001\000\000\000\208\224\148\212A"))
      ((sexp ((2222-11-22 05:00:00.000000Z)))
       (bin_io "\001\000\000\000\181\189\186\253A"))
      ((sexp ((2222-11-22 05:00:00.000001Z)))
       (bin_io "\001\001\000\000\181\189\186\253A"))
      ((sexp ((2222-11-22 05:00:00.001000Z)))
       (bin_io "\001\025\004\000\181\189\186\253A"))
      ((sexp ((2222-11-22 05:00:01.000000Z)))
       (bin_io "\001\000\000\016\181\189\186\253A"))
      ((sexp ((2222-11-22 05:01:00.000000Z)))
       (bin_io "\001\000\000\192\184\189\186\253A"))
      ((sexp ((2222-11-22 06:00:00.000000Z)))
       (bin_io "\001\000\000\000\150\190\186\253A"))
      ((sexp ((2222-11-22 17:00:00.000000Z)))
       (bin_io "\001\000\000\000A\200\186\253A"))
      ((sexp ((2222-11-22 18:42:23.622933Z)))
       (bin_io "\001\137\247\249\192\201\186\253A"))
      ((sexp ((2222-11-23 04:59:59.999999Z)))
       (bin_io "\001\255\255\255\204\210\186\253A"))
      ((sexp ((2222-11-23 05:00:00.000000Z)))
       (bin_io "\001\000\000\000\205\210\186\253A"))
      ((sexp ((1675-11-17 21:17:07.905443Z)))
       (bin_io "\001\167\193`=\199I\001\194"))
      ((sexp ((1694-11-03 04:35:57.669968Z)))
       (bin_io "\001\232\163\146\234v,\000\194"))
      ((sexp ((1765-07-10 06:18:45.148142Z)))
       (bin_io "\0016\161\173\019\192\t\248\193"))
      ((sexp ((1801-06-12 18:58:03.068971Z)))
       (bin_io "\001\127\229N\244\166\208\243\193"))
      ((sexp ((1885-04-01 03:46:16.830498Z)))
       (bin_io "\001\144l\229n3\237\227\193"))
      ((sexp ((1998-08-26 14:41:51.551338Z)))
       (bin_io "\001@\146\198W\015\242\202A"))
      ((sexp ((2037-10-03 15:20:43.521111Z)))
       (bin_io "\001\224Y\225\018\146\220\223A"))
      ((sexp ((2076-09-15 12:19:49.513504Z)))
       (bin_io "\001\160n\176L\205\022\233A"))
      ((sexp ((2081-01-03 22:06:45.819349Z)))
       (bin_io "\001\0288\186\014\195\025\234A"))
      ((sexp ((2175-12-01 15:47:07.694916Z)))
       (bin_io "\001`\030\187\183\0215\248A"))
      ((sexp (
         (1675-11-17 21:17:07.905443Z)
         (1694-11-03 04:35:57.669968Z)
         (1765-07-10 06:18:45.148142Z)
         (1801-06-12 18:58:03.068971Z)
         (1885-04-01 03:46:16.830498Z)
         (1912-06-23 05:00:00.000000Z)
         (1912-06-23 05:00:00.000001Z)
         (1912-06-23 05:00:00.001000Z)
         (1912-06-23 05:00:01.000000Z)
         (1912-06-23 05:01:00.000000Z)
         (1912-06-23 06:00:00.000000Z)
         (1912-06-23 17:00:00.000000Z)
         (1912-06-23 18:42:23.622933Z)
         (1912-06-24 04:59:59.999999Z)
         (1912-06-24 05:00:00.000000Z)
         (1970-01-01 05:00:00.000000Z)
         (1970-01-01 05:00:00.000001Z)
         (1970-01-01 05:00:00.001000Z)
         (1970-01-01 05:00:01.000000Z)
         (1970-01-01 05:01:00.000000Z)
         (1970-01-01 06:00:00.000000Z)
         (1970-01-01 17:00:00.000000Z)
         (1970-01-01 18:42:23.622933Z)
         (1970-01-02 04:59:59.999999Z)
         (1970-01-02 05:00:00.000000Z)
         (1998-08-26 14:41:51.551338Z)
         (1999-12-31 05:00:00.000000Z)
         (1999-12-31 05:00:00.000001Z)
         (1999-12-31 05:00:00.001000Z)
         (1999-12-31 05:00:01.000000Z)
         (1999-12-31 05:01:00.000000Z)
         (1999-12-31 06:00:00.000000Z)
         (1999-12-31 17:00:00.000000Z)
         (1999-12-31 18:42:23.622933Z)
         (2000-01-01 04:59:59.999999Z)
         (2000-01-01 05:00:00.000000Z)
         (2000-01-01 05:00:00.000001Z)
         (2000-01-01 05:00:00.001000Z)
         (2000-01-01 05:00:01.000000Z)
         (2000-01-01 05:01:00.000000Z)
         (2000-01-01 06:00:00.000000Z)
         (2000-01-01 17:00:00.000000Z)
         (2000-01-01 18:42:23.622933Z)
         (2000-01-02 04:59:59.999999Z)
         (2000-01-02 05:00:00.000000Z)
         (2013-10-07 04:00:00.000000Z)
         (2013-10-07 04:00:00.000001Z)
         (2013-10-07 04:00:00.001000Z)
         (2013-10-07 04:00:01.000000Z)
         (2013-10-07 04:01:00.000000Z)
         (2013-10-07 05:00:00.000000Z)
         (2013-10-07 16:00:00.000000Z)
         (2013-10-07 17:42:23.622933Z)
         (2013-10-08 03:59:59.999999Z)
         (2013-10-08 04:00:00.000000Z)
         (2037-10-03 15:20:43.521111Z)
         (2076-09-15 12:19:49.513504Z)
         (2081-01-03 22:06:45.819349Z)
         (2175-12-01 15:47:07.694916Z)
         (2222-11-22 05:00:00.000000Z)
         (2222-11-22 05:00:00.000001Z)
         (2222-11-22 05:00:00.001000Z)
         (2222-11-22 05:00:01.000000Z)
         (2222-11-22 05:01:00.000000Z)
         (2222-11-22 06:00:00.000000Z)
         (2222-11-22 17:00:00.000000Z)
         (2222-11-22 18:42:23.622933Z)
         (2222-11-23 04:59:59.999999Z)
         (2222-11-23 05:00:00.000000Z)))
       (bin_io
        "E\167\193`=\199I\001\194\232\163\146\234v,\000\1946\161\173\019\192\t\248\193\127\229N\244\166\208\243\193\144l\229n3\237\227\193\000\000\000\140\241\012\219\193\252\255\255\139\241\012\219\193\158\239\255\139\241\012\219\193\000\000\192\139\241\012\219\193\000\000\000}\241\012\219\193\000\000\000\b\238\012\219\193\000\000\000\\\199\012\219\193\222!\024\\\193\012\219\193\004\000\000,\157\012\219\193\000\000\000,\157\012\219\193\000\000\000\000\000\148\209@\1901\004\000\000\148\209@\211Mb\016\000\148\209@\000\000\000\000@\148\209@\000\000\000\000\000\163\209@\000\000\000\000\000\024\213@\000\000\000\000\000\226\237@\234\151\136\247\249p\240@\145\243\254\255\255|\249@\000\000\000\000\000}\249@@\146\198W\015\242\202A\000\000\000(\0286\204A\b\000\000(\0286\204A\197 \000(\0286\204A\000\000\128(\0286\204A\000\000\000F\0286\204A\000\000\0000#6\204A\000\000\000\136p6\204AE\188\207\135|6\204A\248\255\255\231\1966\204A\000\000\000\232\1966\204A\b\000\000\232\1966\204A\197 \000\232\1966\204A\000\000\128\232\1966\204A\000\000\000\006\1976\204A\000\000\000\240\2036\204A\000\000\000H\0257\204AE\188\207G%7\204A\248\255\255\167m7\204A\000\000\000\168m7\204A\000\000\000p\140\148\212A\004\000\000p\140\148\212Ab\016\000p\140\148\212A\000\000@p\140\148\212A\000\000\000\127\140\148\212A\000\000\000\244\143\148\212A\000\000\000\160\182\148\212A\"\222\231\159\188\148\212A\252\255\255\207\224\148\212A\000\000\000\208\224\148\212A\224Y\225\018\146\220\223A\160n\176L\205\022\233A\0288\186\014\195\025\234A`\030\187\183\0215\248A\000\000\000\181\189\186\253A\001\000\000\181\189\186\253A\025\004\000\181\189\186\253A\000\000\016\181\189\186\253A\000\000\192\184\189\186\253A\000\000\000\150\190\186\253A\000\000\000A\200\186\253A\137\247\249\192\201\186\253A\255\255\255\204\210\186\253A\000\000\000\205\210\186\253A"))
      |}]
  ;;

  let%expect_test "With_utc_sexp.V2.Map" =
    test_stability
      (module struct
        include Specialize_to_int (Time.Stable.With_utc_sexp.V2.Map)
        include For_map
      end);
    [%expect
      {|
      (bin_shape_digest 31404094f08cdbe1f9fca07a1a1e5303)
      ((sexp ()) (bin_io "\000"))
      ((sexp (((1912-06-23 05:00:00.000000Z) 0)))
       (bin_io "\001\000\000\000\140\241\012\219\193\000"))
      ((sexp (((1912-06-23 05:00:00.000001Z) 1)))
       (bin_io "\001\252\255\255\139\241\012\219\193\001"))
      ((sexp (((1912-06-23 05:00:00.001000Z) 2)))
       (bin_io "\001\158\239\255\139\241\012\219\193\002"))
      ((sexp (((1912-06-23 05:00:01.000000Z) 3)))
       (bin_io "\001\000\000\192\139\241\012\219\193\003"))
      ((sexp (((1912-06-23 05:01:00.000000Z) 4)))
       (bin_io "\001\000\000\000}\241\012\219\193\004"))
      ((sexp (((1912-06-23 06:00:00.000000Z) 5)))
       (bin_io "\001\000\000\000\b\238\012\219\193\005"))
      ((sexp (((1912-06-23 17:00:00.000000Z) 6)))
       (bin_io "\001\000\000\000\\\199\012\219\193\006"))
      ((sexp (((1912-06-23 18:42:23.622933Z) 7)))
       (bin_io "\001\222!\024\\\193\012\219\193\007"))
      ((sexp (((1912-06-24 04:59:59.999999Z) 8)))
       (bin_io "\001\004\000\000,\157\012\219\193\b"))
      ((sexp (((1912-06-24 05:00:00.000000Z) 9)))
       (bin_io "\001\000\000\000,\157\012\219\193\t"))
      ((sexp (((1970-01-01 05:00:00.000000Z) 10)))
       (bin_io "\001\000\000\000\000\000\148\209@\n"))
      ((sexp (((1970-01-01 05:00:00.000001Z) 11)))
       (bin_io "\001\1901\004\000\000\148\209@\011"))
      ((sexp (((1970-01-01 05:00:00.001000Z) 12)))
       (bin_io "\001\211Mb\016\000\148\209@\012"))
      ((sexp (((1970-01-01 05:00:01.000000Z) 13)))
       (bin_io "\001\000\000\000\000@\148\209@\r"))
      ((sexp (((1970-01-01 05:01:00.000000Z) 14)))
       (bin_io "\001\000\000\000\000\000\163\209@\014"))
      ((sexp (((1970-01-01 06:00:00.000000Z) 15)))
       (bin_io "\001\000\000\000\000\000\024\213@\015"))
      ((sexp (((1970-01-01 17:00:00.000000Z) 16)))
       (bin_io "\001\000\000\000\000\000\226\237@\016"))
      ((sexp (((1970-01-01 18:42:23.622933Z) 17)))
       (bin_io "\001\234\151\136\247\249p\240@\017"))
      ((sexp (((1970-01-02 04:59:59.999999Z) 18)))
       (bin_io "\001\145\243\254\255\255|\249@\018"))
      ((sexp (((1970-01-02 05:00:00.000000Z) 19)))
       (bin_io "\001\000\000\000\000\000}\249@\019"))
      ((sexp (((1999-12-31 05:00:00.000000Z) 20)))
       (bin_io "\001\000\000\000(\0286\204A\020"))
      ((sexp (((1999-12-31 05:00:00.000001Z) 21)))
       (bin_io "\001\b\000\000(\0286\204A\021"))
      ((sexp (((1999-12-31 05:00:00.001000Z) 22)))
       (bin_io "\001\197 \000(\0286\204A\022"))
      ((sexp (((1999-12-31 05:00:01.000000Z) 23)))
       (bin_io "\001\000\000\128(\0286\204A\023"))
      ((sexp (((1999-12-31 05:01:00.000000Z) 24)))
       (bin_io "\001\000\000\000F\0286\204A\024"))
      ((sexp (((1999-12-31 06:00:00.000000Z) 25)))
       (bin_io "\001\000\000\0000#6\204A\025"))
      ((sexp (((1999-12-31 17:00:00.000000Z) 26)))
       (bin_io "\001\000\000\000\136p6\204A\026"))
      ((sexp (((1999-12-31 18:42:23.622933Z) 27)))
       (bin_io "\001E\188\207\135|6\204A\027"))
      ((sexp (((2000-01-01 04:59:59.999999Z) 28)))
       (bin_io "\001\248\255\255\231\1966\204A\028"))
      ((sexp (((2000-01-01 05:00:00.000000Z) 29)))
       (bin_io "\001\000\000\000\232\1966\204A\029"))
      ((sexp (((2000-01-01 05:00:00.000001Z) 30)))
       (bin_io "\001\b\000\000\232\1966\204A\030"))
      ((sexp (((2000-01-01 05:00:00.001000Z) 31)))
       (bin_io "\001\197 \000\232\1966\204A\031"))
      ((sexp (((2000-01-01 05:00:01.000000Z) 32)))
       (bin_io "\001\000\000\128\232\1966\204A "))
      ((sexp (((2000-01-01 05:01:00.000000Z) 33)))
       (bin_io "\001\000\000\000\006\1976\204A!"))
      ((sexp (((2000-01-01 06:00:00.000000Z) 34)))
       (bin_io "\001\000\000\000\240\2036\204A\""))
      ((sexp (((2000-01-01 17:00:00.000000Z) 35)))
       (bin_io "\001\000\000\000H\0257\204A#"))
      ((sexp (((2000-01-01 18:42:23.622933Z) 36)))
       (bin_io "\001E\188\207G%7\204A$"))
      ((sexp (((2000-01-02 04:59:59.999999Z) 37)))
       (bin_io "\001\248\255\255\167m7\204A%"))
      ((sexp (((2000-01-02 05:00:00.000000Z) 38)))
       (bin_io "\001\000\000\000\168m7\204A&"))
      ((sexp (((2013-10-07 04:00:00.000000Z) 39)))
       (bin_io "\001\000\000\000p\140\148\212A'"))
      ((sexp (((2013-10-07 04:00:00.000001Z) 40)))
       (bin_io "\001\004\000\000p\140\148\212A("))
      ((sexp (((2013-10-07 04:00:00.001000Z) 41)))
       (bin_io "\001b\016\000p\140\148\212A)"))
      ((sexp (((2013-10-07 04:00:01.000000Z) 42)))
       (bin_io "\001\000\000@p\140\148\212A*"))
      ((sexp (((2013-10-07 04:01:00.000000Z) 43)))
       (bin_io "\001\000\000\000\127\140\148\212A+"))
      ((sexp (((2013-10-07 05:00:00.000000Z) 44)))
       (bin_io "\001\000\000\000\244\143\148\212A,"))
      ((sexp (((2013-10-07 16:00:00.000000Z) 45)))
       (bin_io "\001\000\000\000\160\182\148\212A-"))
      ((sexp (((2013-10-07 17:42:23.622933Z) 46)))
       (bin_io "\001\"\222\231\159\188\148\212A."))
      ((sexp (((2013-10-08 03:59:59.999999Z) 47)))
       (bin_io "\001\252\255\255\207\224\148\212A/"))
      ((sexp (((2013-10-08 04:00:00.000000Z) 48)))
       (bin_io "\001\000\000\000\208\224\148\212A0"))
      ((sexp (((2222-11-22 05:00:00.000000Z) 49)))
       (bin_io "\001\000\000\000\181\189\186\253A1"))
      ((sexp (((2222-11-22 05:00:00.000001Z) 50)))
       (bin_io "\001\001\000\000\181\189\186\253A2"))
      ((sexp (((2222-11-22 05:00:00.001000Z) 51)))
       (bin_io "\001\025\004\000\181\189\186\253A3"))
      ((sexp (((2222-11-22 05:00:01.000000Z) 52)))
       (bin_io "\001\000\000\016\181\189\186\253A4"))
      ((sexp (((2222-11-22 05:01:00.000000Z) 53)))
       (bin_io "\001\000\000\192\184\189\186\253A5"))
      ((sexp (((2222-11-22 06:00:00.000000Z) 54)))
       (bin_io "\001\000\000\000\150\190\186\253A6"))
      ((sexp (((2222-11-22 17:00:00.000000Z) 55)))
       (bin_io "\001\000\000\000A\200\186\253A7"))
      ((sexp (((2222-11-22 18:42:23.622933Z) 56)))
       (bin_io "\001\137\247\249\192\201\186\253A8"))
      ((sexp (((2222-11-23 04:59:59.999999Z) 57)))
       (bin_io "\001\255\255\255\204\210\186\253A9"))
      ((sexp (((2222-11-23 05:00:00.000000Z) 58)))
       (bin_io "\001\000\000\000\205\210\186\253A:"))
      ((sexp (((1675-11-17 21:17:07.905443Z) 59)))
       (bin_io "\001\167\193`=\199I\001\194;"))
      ((sexp (((1694-11-03 04:35:57.669968Z) 60)))
       (bin_io "\001\232\163\146\234v,\000\194<"))
      ((sexp (((1765-07-10 06:18:45.148142Z) 61)))
       (bin_io "\0016\161\173\019\192\t\248\193="))
      ((sexp (((1801-06-12 18:58:03.068971Z) 62)))
       (bin_io "\001\127\229N\244\166\208\243\193>"))
      ((sexp (((1885-04-01 03:46:16.830498Z) 63)))
       (bin_io "\001\144l\229n3\237\227\193?"))
      ((sexp (((1998-08-26 14:41:51.551338Z) 64)))
       (bin_io "\001@\146\198W\015\242\202A@"))
      ((sexp (((2037-10-03 15:20:43.521111Z) 65)))
       (bin_io "\001\224Y\225\018\146\220\223AA"))
      ((sexp (((2076-09-15 12:19:49.513504Z) 66)))
       (bin_io "\001\160n\176L\205\022\233AB"))
      ((sexp (((2081-01-03 22:06:45.819349Z) 67)))
       (bin_io "\001\0288\186\014\195\025\234AC"))
      ((sexp (((2175-12-01 15:47:07.694916Z) 68)))
       (bin_io "\001`\030\187\183\0215\248AD"))
      ((sexp (
         ((1675-11-17 21:17:07.905443Z) 59)
         ((1694-11-03 04:35:57.669968Z) 60)
         ((1765-07-10 06:18:45.148142Z) 61)
         ((1801-06-12 18:58:03.068971Z) 62)
         ((1885-04-01 03:46:16.830498Z) 63)
         ((1912-06-23 05:00:00.000000Z) 0)
         ((1912-06-23 05:00:00.000001Z) 1)
         ((1912-06-23 05:00:00.001000Z) 2)
         ((1912-06-23 05:00:01.000000Z) 3)
         ((1912-06-23 05:01:00.000000Z) 4)
         ((1912-06-23 06:00:00.000000Z) 5)
         ((1912-06-23 17:00:00.000000Z) 6)
         ((1912-06-23 18:42:23.622933Z) 7)
         ((1912-06-24 04:59:59.999999Z) 8)
         ((1912-06-24 05:00:00.000000Z) 9)
         ((1970-01-01 05:00:00.000000Z) 10)
         ((1970-01-01 05:00:00.000001Z) 11)
         ((1970-01-01 05:00:00.001000Z) 12)
         ((1970-01-01 05:00:01.000000Z) 13)
         ((1970-01-01 05:01:00.000000Z) 14)
         ((1970-01-01 06:00:00.000000Z) 15)
         ((1970-01-01 17:00:00.000000Z) 16)
         ((1970-01-01 18:42:23.622933Z) 17)
         ((1970-01-02 04:59:59.999999Z) 18)
         ((1970-01-02 05:00:00.000000Z) 19)
         ((1998-08-26 14:41:51.551338Z) 64)
         ((1999-12-31 05:00:00.000000Z) 20)
         ((1999-12-31 05:00:00.000001Z) 21)
         ((1999-12-31 05:00:00.001000Z) 22)
         ((1999-12-31 05:00:01.000000Z) 23)
         ((1999-12-31 05:01:00.000000Z) 24)
         ((1999-12-31 06:00:00.000000Z) 25)
         ((1999-12-31 17:00:00.000000Z) 26)
         ((1999-12-31 18:42:23.622933Z) 27)
         ((2000-01-01 04:59:59.999999Z) 28)
         ((2000-01-01 05:00:00.000000Z) 29)
         ((2000-01-01 05:00:00.000001Z) 30)
         ((2000-01-01 05:00:00.001000Z) 31)
         ((2000-01-01 05:00:01.000000Z) 32)
         ((2000-01-01 05:01:00.000000Z) 33)
         ((2000-01-01 06:00:00.000000Z) 34)
         ((2000-01-01 17:00:00.000000Z) 35)
         ((2000-01-01 18:42:23.622933Z) 36)
         ((2000-01-02 04:59:59.999999Z) 37)
         ((2000-01-02 05:00:00.000000Z) 38)
         ((2013-10-07 04:00:00.000000Z) 39)
         ((2013-10-07 04:00:00.000001Z) 40)
         ((2013-10-07 04:00:00.001000Z) 41)
         ((2013-10-07 04:00:01.000000Z) 42)
         ((2013-10-07 04:01:00.000000Z) 43)
         ((2013-10-07 05:00:00.000000Z) 44)
         ((2013-10-07 16:00:00.000000Z) 45)
         ((2013-10-07 17:42:23.622933Z) 46)
         ((2013-10-08 03:59:59.999999Z) 47)
         ((2013-10-08 04:00:00.000000Z) 48)
         ((2037-10-03 15:20:43.521111Z) 65)
         ((2076-09-15 12:19:49.513504Z) 66)
         ((2081-01-03 22:06:45.819349Z) 67)
         ((2175-12-01 15:47:07.694916Z) 68)
         ((2222-11-22 05:00:00.000000Z) 49)
         ((2222-11-22 05:00:00.000001Z) 50)
         ((2222-11-22 05:00:00.001000Z) 51)
         ((2222-11-22 05:00:01.000000Z) 52)
         ((2222-11-22 05:01:00.000000Z) 53)
         ((2222-11-22 06:00:00.000000Z) 54)
         ((2222-11-22 17:00:00.000000Z) 55)
         ((2222-11-22 18:42:23.622933Z) 56)
         ((2222-11-23 04:59:59.999999Z) 57)
         ((2222-11-23 05:00:00.000000Z) 58)))
       (bin_io
        "E\167\193`=\199I\001\194;\232\163\146\234v,\000\194<6\161\173\019\192\t\248\193=\127\229N\244\166\208\243\193>\144l\229n3\237\227\193?\000\000\000\140\241\012\219\193\000\252\255\255\139\241\012\219\193\001\158\239\255\139\241\012\219\193\002\000\000\192\139\241\012\219\193\003\000\000\000}\241\012\219\193\004\000\000\000\b\238\012\219\193\005\000\000\000\\\199\012\219\193\006\222!\024\\\193\012\219\193\007\004\000\000,\157\012\219\193\b\000\000\000,\157\012\219\193\t\000\000\000\000\000\148\209@\n\1901\004\000\000\148\209@\011\211Mb\016\000\148\209@\012\000\000\000\000@\148\209@\r\000\000\000\000\000\163\209@\014\000\000\000\000\000\024\213@\015\000\000\000\000\000\226\237@\016\234\151\136\247\249p\240@\017\145\243\254\255\255|\249@\018\000\000\000\000\000}\249@\019@\146\198W\015\242\202A@\000\000\000(\0286\204A\020\b\000\000(\0286\204A\021\197 \000(\0286\204A\022\000\000\128(\0286\204A\023\000\000\000F\0286\204A\024\000\000\0000#6\204A\025\000\000\000\136p6\204A\026E\188\207\135|6\204A\027\248\255\255\231\1966\204A\028\000\000\000\232\1966\204A\029\b\000\000\232\1966\204A\030\197 \000\232\1966\204A\031\000\000\128\232\1966\204A \000\000\000\006\1976\204A!\000\000\000\240\2036\204A\"\000\000\000H\0257\204A#E\188\207G%7\204A$\248\255\255\167m7\204A%\000\000\000\168m7\204A&\000\000\000p\140\148\212A'\004\000\000p\140\148\212A(b\016\000p\140\148\212A)\000\000@p\140\148\212A*\000\000\000\127\140\148\212A+\000\000\000\244\143\148\212A,\000\000\000\160\182\148\212A-\"\222\231\159\188\148\212A.\252\255\255\207\224\148\212A/\000\000\000\208\224\148\212A0\224Y\225\018\146\220\223AA\160n\176L\205\022\233AB\0288\186\014\195\025\234AC`\030\187\183\0215\248AD\000\000\000\181\189\186\253A1\001\000\000\181\189\186\253A2\025\004\000\181\189\186\253A3\000\000\016\181\189\186\253A4\000\000\192\184\189\186\253A5\000\000\000\150\190\186\253A6\000\000\000A\200\186\253A7\137\247\249\192\201\186\253A8\255\255\255\204\210\186\253A9\000\000\000\205\210\186\253A:"))
      |}]
  ;;

  (* [With_t_of_sexp_abs] *)

  let%expect_test "With_t_of_sexp_abs.V1" =
    test_stability
      (module struct
        include Time.Stable.With_t_of_sexp_abs.V1
        include For_time
      end);
    [%expect
      {|
      (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
      ((sexp (1912-06-23 00:00:00.000000-05:00))
       (bin_io "\000\000\000\140\241\012\219\193"))
      ((sexp (1912-06-23 00:00:00.000001-05:00))
       (bin_io "\252\255\255\139\241\012\219\193"))
      ((sexp (1912-06-23 00:00:00.001000-05:00))
       (bin_io "\158\239\255\139\241\012\219\193"))
      ((sexp (1912-06-23 00:00:01.000000-05:00))
       (bin_io "\000\000\192\139\241\012\219\193"))
      ((sexp (1912-06-23 00:01:00.000000-05:00))
       (bin_io "\000\000\000}\241\012\219\193"))
      ((sexp (1912-06-23 01:00:00.000000-05:00))
       (bin_io "\000\000\000\b\238\012\219\193"))
      ((sexp (1912-06-23 12:00:00.000000-05:00))
       (bin_io "\000\000\000\\\199\012\219\193"))
      ((sexp (1912-06-23 13:42:23.622933-05:00))
       (bin_io "\222!\024\\\193\012\219\193"))
      ((sexp (1912-06-23 23:59:59.999999-05:00))
       (bin_io "\004\000\000,\157\012\219\193"))
      ((sexp (1912-06-24 00:00:00.000000-05:00))
       (bin_io "\000\000\000,\157\012\219\193"))
      ((sexp (1970-01-01 00:00:00.000000-05:00))
       (bin_io "\000\000\000\000\000\148\209@"))
      ((sexp (1970-01-01 00:00:00.000001-05:00))
       (bin_io "\1901\004\000\000\148\209@"))
      ((sexp (1970-01-01 00:00:00.001000-05:00)) (bin_io "\211Mb\016\000\148\209@"))
      ((sexp (1970-01-01 00:00:01.000000-05:00))
       (bin_io "\000\000\000\000@\148\209@"))
      ((sexp (1970-01-01 00:01:00.000000-05:00))
       (bin_io "\000\000\000\000\000\163\209@"))
      ((sexp (1970-01-01 01:00:00.000000-05:00))
       (bin_io "\000\000\000\000\000\024\213@"))
      ((sexp (1970-01-01 12:00:00.000000-05:00))
       (bin_io "\000\000\000\000\000\226\237@"))
      ((sexp (1970-01-01 13:42:23.622933-05:00))
       (bin_io "\234\151\136\247\249p\240@"))
      ((sexp (1970-01-01 23:59:59.999999-05:00))
       (bin_io "\145\243\254\255\255|\249@"))
      ((sexp (1970-01-02 00:00:00.000000-05:00))
       (bin_io "\000\000\000\000\000}\249@"))
      ((sexp (1999-12-31 00:00:00.000000-05:00)) (bin_io "\000\000\000(\0286\204A"))
      ((sexp (1999-12-31 00:00:00.000001-05:00)) (bin_io "\b\000\000(\0286\204A"))
      ((sexp (1999-12-31 00:00:00.001000-05:00)) (bin_io "\197 \000(\0286\204A"))
      ((sexp (1999-12-31 00:00:01.000000-05:00)) (bin_io "\000\000\128(\0286\204A"))
      ((sexp (1999-12-31 00:01:00.000000-05:00)) (bin_io "\000\000\000F\0286\204A"))
      ((sexp (1999-12-31 01:00:00.000000-05:00)) (bin_io "\000\000\0000#6\204A"))
      ((sexp (1999-12-31 12:00:00.000000-05:00)) (bin_io "\000\000\000\136p6\204A"))
      ((sexp (1999-12-31 13:42:23.622933-05:00)) (bin_io "E\188\207\135|6\204A"))
      ((sexp (1999-12-31 23:59:59.999999-05:00))
       (bin_io "\248\255\255\231\1966\204A"))
      ((sexp (2000-01-01 00:00:00.000000-05:00))
       (bin_io "\000\000\000\232\1966\204A"))
      ((sexp (2000-01-01 00:00:00.000001-05:00))
       (bin_io "\b\000\000\232\1966\204A"))
      ((sexp (2000-01-01 00:00:00.001000-05:00)) (bin_io "\197 \000\232\1966\204A"))
      ((sexp (2000-01-01 00:00:01.000000-05:00))
       (bin_io "\000\000\128\232\1966\204A"))
      ((sexp (2000-01-01 00:01:00.000000-05:00))
       (bin_io "\000\000\000\006\1976\204A"))
      ((sexp (2000-01-01 01:00:00.000000-05:00))
       (bin_io "\000\000\000\240\2036\204A"))
      ((sexp (2000-01-01 12:00:00.000000-05:00)) (bin_io "\000\000\000H\0257\204A"))
      ((sexp (2000-01-01 13:42:23.622933-05:00)) (bin_io "E\188\207G%7\204A"))
      ((sexp (2000-01-01 23:59:59.999999-05:00)) (bin_io "\248\255\255\167m7\204A"))
      ((sexp (2000-01-02 00:00:00.000000-05:00)) (bin_io "\000\000\000\168m7\204A"))
      ((sexp (2013-10-07 00:00:00.000000-04:00))
       (bin_io "\000\000\000p\140\148\212A"))
      ((sexp (2013-10-07 00:00:00.000001-04:00))
       (bin_io "\004\000\000p\140\148\212A"))
      ((sexp (2013-10-07 00:00:00.001000-04:00)) (bin_io "b\016\000p\140\148\212A"))
      ((sexp (2013-10-07 00:00:01.000000-04:00)) (bin_io "\000\000@p\140\148\212A"))
      ((sexp (2013-10-07 00:01:00.000000-04:00))
       (bin_io "\000\000\000\127\140\148\212A"))
      ((sexp (2013-10-07 01:00:00.000000-04:00))
       (bin_io "\000\000\000\244\143\148\212A"))
      ((sexp (2013-10-07 12:00:00.000000-04:00))
       (bin_io "\000\000\000\160\182\148\212A"))
      ((sexp (2013-10-07 13:42:23.622933-04:00))
       (bin_io "\"\222\231\159\188\148\212A"))
      ((sexp (2013-10-07 23:59:59.999999-04:00))
       (bin_io "\252\255\255\207\224\148\212A"))
      ((sexp (2013-10-08 00:00:00.000000-04:00))
       (bin_io "\000\000\000\208\224\148\212A"))
      ((sexp (2222-11-22 00:00:00.000000-05:00))
       (bin_io "\000\000\000\181\189\186\253A"))
      ((sexp (2222-11-22 00:00:00.000001-05:00))
       (bin_io "\001\000\000\181\189\186\253A"))
      ((sexp (2222-11-22 00:00:00.001000-05:00))
       (bin_io "\025\004\000\181\189\186\253A"))
      ((sexp (2222-11-22 00:00:01.000000-05:00))
       (bin_io "\000\000\016\181\189\186\253A"))
      ((sexp (2222-11-22 00:01:00.000000-05:00))
       (bin_io "\000\000\192\184\189\186\253A"))
      ((sexp (2222-11-22 01:00:00.000000-05:00))
       (bin_io "\000\000\000\150\190\186\253A"))
      ((sexp (2222-11-22 12:00:00.000000-05:00))
       (bin_io "\000\000\000A\200\186\253A"))
      ((sexp (2222-11-22 13:42:23.622933-05:00))
       (bin_io "\137\247\249\192\201\186\253A"))
      ((sexp (2222-11-22 23:59:59.999999-05:00))
       (bin_io "\255\255\255\204\210\186\253A"))
      ((sexp (2222-11-23 00:00:00.000000-05:00))
       (bin_io "\000\000\000\205\210\186\253A"))
      ((sexp (1675-11-17 16:21:05.905443-04:56:02))
       (bin_io "\167\193`=\199I\001\194"))
      ((sexp (1694-11-02 23:39:55.669968-04:56:02))
       (bin_io "\232\163\146\234v,\000\194"))
      ((sexp (1765-07-10 01:22:43.148142-04:56:02))
       (bin_io "6\161\173\019\192\t\248\193"))
      ((sexp (1801-06-12 14:02:01.068971-04:56:02))
       (bin_io "\127\229N\244\166\208\243\193"))
      ((sexp (1885-03-31 22:46:16.830498-05:00)) (bin_io "\144l\229n3\237\227\193"))
      ((sexp (1998-08-26 10:41:51.551338-04:00)) (bin_io "@\146\198W\015\242\202A"))
      ((sexp (2037-10-03 11:20:43.521111-04:00))
       (bin_io "\224Y\225\018\146\220\223A"))
      ((sexp (2076-09-15 07:19:49.513504-05:00)) (bin_io "\160n\176L\205\022\233A"))
      ((sexp (2081-01-03 17:06:45.819349-05:00))
       (bin_io "\0288\186\014\195\025\234A"))
      ((sexp (2175-12-01 10:47:07.694916-05:00)) (bin_io "`\030\187\183\0215\248A"))
      |}];
    show_raise (fun () ->
      Time.Stable.With_t_of_sexp_abs.V1.t_of_sexp
        (Sexp.of_string "(2000-01-01 00:00:00.000000)"));
    [%expect
      {|
      (raised (
        Of_sexp_error
        "Time.t_of_sexp: (time.ml.Make.Time_of_string \"2000-01-01 00:00:00.000000\"\n  (time_float.ml.Time_string_not_absolute \"2000-01-01 00:00:00.000000\"))"
        (invalid_sexp (2000-01-01 00:00:00.000000))))
      |}]
  ;;
end

module%test [@name "Time.Stable.Span"] _ = struct
  let units =
    [ Span.nanosecond
    ; Span.microsecond
    ; Span.millisecond
    ; Span.second
    ; Span.minute
    ; Span.hour
    ; Span.day
    ]
  ;;

  let examples = [ Span.zero ] @ units @ [ List.sum (module Span) units ~f:Fn.id ]

  let%expect_test "V1" =
    print_and_check_stable_type
    (* V1 round-trips imprecisely in some cases, so we document them and note that
           they are still reasonably close. *)
      ~cr:Comment
      (module Time.Stable.Span.V1)
      examples;
    [%expect
      {|
      (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
      ((sexp   0s)
       (bin_io "\000\000\000\000\000\000\000\000"))
      ((sexp   1e-06ms)
       (bin_io "\149\214&\232\011.\017>"))
      (* require-failed: lib/time_float_unix/test/test_time_unix.ml:LINE:COL. *)
      ("sexp serialization failed to round-trip"
        (original       1e-06ms)
        (sexp           1e-06ms)
        (sexp_roundtrip 1e-06ms))
      ((sexp   0.001ms)
       (bin_io "\141\237\181\160\247\198\176>"))
      ((sexp   1ms)
       (bin_io "\252\169\241\210MbP?"))
      ((sexp   1s)
       (bin_io "\000\000\000\000\000\000\240?"))
      ((sexp   1m)
       (bin_io "\000\000\000\000\000\000N@"))
      ((sexp   1h)
       (bin_io "\000\000\000\000\000 \172@"))
      ((sexp   1d)
       (bin_io "\000\000\000\000\000\024\245@"))
      ((sexp   1.04237d)
       (bin_io ")\160\025\004\208\252\245@"))
      (* require-failed: lib/time_float_unix/test/test_time_unix.ml:LINE:COL. *)
      ("sexp serialization failed to round-trip"
        (original       1.04237d)
        (sexp           1.04237d)
        (sexp_roundtrip 1.04237d))
      |}]
  ;;

  let%expect_test "V2" =
    print_and_check_stable_type (module Time.Stable.Span.V2) examples;
    [%expect
      {|
      (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
      ((sexp   0s)
       (bin_io "\000\000\000\000\000\000\000\000"))
      ((sexp   1ns)
       (bin_io "\149\214&\232\011.\017>"))
      ((sexp   1us)
       (bin_io "\141\237\181\160\247\198\176>"))
      ((sexp   1ms)
       (bin_io "\252\169\241\210MbP?"))
      ((sexp   1s)
       (bin_io "\000\000\000\000\000\000\240?"))
      ((sexp   1m)
       (bin_io "\000\000\000\000\000\000N@"))
      ((sexp   1h)
       (bin_io "\000\000\000\000\000 \172@"))
      ((sexp   1d)
       (bin_io "\000\000\000\000\000\024\245@"))
      ((sexp   1.0423726967708449d)
       (bin_io ")\160\025\004\208\252\245@"))
      |}]
  ;;
end

module%test [@name "Time.Stable.Ofday"] _ = struct
  let examples =
    [ Ofday.start_of_day
    ; Ofday.create ~hr:12 ()
    ; Ofday.create ~hr:23 ~min:59 ~sec:29 ~ms:999 ~us:999 () (* individual units *)
    ; Ofday.create ~us:1 ()
    ; Ofday.create ~ms:1 ()
    ; Ofday.create ~sec:1 ()
    ; Ofday.create ~min:1 ()
    ; Ofday.create ~hr:1 ()
    ; Ofday.create ~hr:24 ()
    ; Ofday.create ~hr:5 ~min:0 ~sec:38 ~ms:770 ~us:479 ()
    ; Ofday.create ~hr:20 ~min:30 ~sec:29 ~ms:782 ~us:106 ()
    ; Ofday.create ~hr:6 ~min:18 ~sec:35 ~ms:327 ~us:424 ()
    ; Ofday.create ~hr:18 ~min:12 ~sec:8 ~ms:192 ~us:141 ()
    ; Ofday.create ~hr:6 ~min:40 ~sec:20 ~ms:750 ~us:241 ()
    ]
  ;;

  let%expect_test "V1" =
    print_and_check_stable_type
    (* V1 round-trips imprecisely in some cases, so we document them and note that
           they are still reasonably close. *)
      ~cr:Comment
      (module Time.Stable.Ofday.V1)
      examples;
    [%expect
      {|
      (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
      ((sexp   00:00:00.000000)
       (bin_io "\000\000\000\000\000\000\000\000"))
      ((sexp   12:00:00.000000)
       (bin_io "\000\000\000\000\000\024\229@"))
      ((sexp   23:59:29.999999)
       (bin_io "\144\243\254\255\031\022\245@"))
      (* require-failed: lib/time_float_unix/test/test_time_unix.ml:LINE:COL. *)
      ("sexp serialization failed to round-trip"
        (original       23:59:29.999999)
        (sexp           23:59:29.999999)
        (sexp_roundtrip 23:59:29.999999))
      ((sexp   00:00:00.000001)
       (bin_io "\141\237\181\160\247\198\176>"))
      ((sexp   00:00:00.001000)
       (bin_io "\252\169\241\210MbP?"))
      ((sexp   00:00:01.000000)
       (bin_io "\000\000\000\000\000\000\240?"))
      ((sexp   00:01:00.000000)
       (bin_io "\000\000\000\000\000\000N@"))
      ((sexp   01:00:00.000000)
       (bin_io "\000\000\000\000\000 \172@"))
      ((sexp   24:00:00.000000)
       (bin_io "\000\000\000\000\000\024\245@"))
      ((sexp   05:00:38.770479)
       (bin_io "\208&\135O\177\157\209@"))
      ((sexp   20:30:29.782106)
       (bin_io "\193\148\129\131\\\006\242@"))
      (* require-failed: lib/time_float_unix/test/test_time_unix.ml:LINE:COL. *)
      ("sexp serialization failed to round-trip"
        (original       20:30:29.782106)
        (sexp           20:30:29.782106)
        (sexp_roundtrip 20:30:29.782106))
      ((sexp   06:18:35.327424)
       (bin_io "\252\202\131\244\212.\214@"))
      (* require-failed: lib/time_float_unix/test/test_time_unix.ml:LINE:COL. *)
      ("sexp serialization failed to round-trip"
        (original       06:18:35.327424)
        (sexp           06:18:35.327424)
        (sexp_roundtrip 06:18:35.327424))
      ((sexp   18:12:08.192141)
       (bin_io "\231\225\004&\006\255\239@"))
      ((sexp   06:40:20.750241)
       (bin_io "\200\211\242\0030u\215@"))
      |}]
  ;;

  let zoned_examples =
    let zone_new_york = Zone.find_exn "America/New_York" in
    List.map examples ~f:(fun example -> Ofday.Zoned.create example Zone.utc)
    @ List.map examples ~f:(fun example -> Ofday.Zoned.create example zone_new_york)
  ;;

  let%expect_test "Zoned.V1" =
    print_and_check_stable_type
    (* V1 round-trips imprecisely in some cases, so we document them and note that
           they are still reasonably close. *)
      ~cr:Comment
      (module Time.Stable.Ofday.Zoned.V1)
      zoned_examples;
    [%expect
      {|
      (bin_shape_digest 490573c3397b4fe37e8ade0086fb4759)
      ((sexp (00:00:00.000000 UTC))
       (bin_io "\000\000\000\000\000\000\000\000\003UTC"))
      ((sexp (12:00:00.000000 UTC)) (bin_io "\000\000\000\000\000\024\229@\003UTC"))
      ((sexp (23:59:29.999999 UTC)) (bin_io "\144\243\254\255\031\022\245@\003UTC"))
      (* require-failed: lib/time_float_unix/test/test_time_unix.ml:LINE:COL. *)
      ("sexp serialization failed to round-trip"
        (original       (23:59:29.999999 UTC))
        (sexp           (23:59:29.999999 UTC))
        (sexp_roundtrip (23:59:29.999999 UTC)))
      ((sexp (00:00:00.000001 UTC)) (bin_io "\141\237\181\160\247\198\176>\003UTC"))
      ((sexp (00:00:00.001000 UTC)) (bin_io "\252\169\241\210MbP?\003UTC"))
      ((sexp (00:00:01.000000 UTC)) (bin_io "\000\000\000\000\000\000\240?\003UTC"))
      ((sexp (00:01:00.000000 UTC)) (bin_io "\000\000\000\000\000\000N@\003UTC"))
      ((sexp (01:00:00.000000 UTC)) (bin_io "\000\000\000\000\000 \172@\003UTC"))
      ((sexp (24:00:00.000000 UTC)) (bin_io "\000\000\000\000\000\024\245@\003UTC"))
      ((sexp (05:00:38.770479 UTC)) (bin_io "\208&\135O\177\157\209@\003UTC"))
      ((sexp (20:30:29.782106 UTC)) (bin_io "\193\148\129\131\\\006\242@\003UTC"))
      (* require-failed: lib/time_float_unix/test/test_time_unix.ml:LINE:COL. *)
      ("sexp serialization failed to round-trip"
        (original       (20:30:29.782106 UTC))
        (sexp           (20:30:29.782106 UTC))
        (sexp_roundtrip (20:30:29.782106 UTC)))
      ((sexp (06:18:35.327424 UTC)) (bin_io "\252\202\131\244\212.\214@\003UTC"))
      (* require-failed: lib/time_float_unix/test/test_time_unix.ml:LINE:COL. *)
      ("sexp serialization failed to round-trip"
        (original       (06:18:35.327424 UTC))
        (sexp           (06:18:35.327424 UTC))
        (sexp_roundtrip (06:18:35.327424 UTC)))
      ((sexp (18:12:08.192141 UTC)) (bin_io "\231\225\004&\006\255\239@\003UTC"))
      ((sexp (06:40:20.750241 UTC)) (bin_io "\200\211\242\0030u\215@\003UTC"))
      ((sexp (00:00:00.000000 America/New_York))
       (bin_io "\000\000\000\000\000\000\000\000\016America/New_York"))
      ((sexp (12:00:00.000000 America/New_York))
       (bin_io "\000\000\000\000\000\024\229@\016America/New_York"))
      ((sexp (23:59:29.999999 America/New_York))
       (bin_io "\144\243\254\255\031\022\245@\016America/New_York"))
      (* require-failed: lib/time_float_unix/test/test_time_unix.ml:LINE:COL. *)
      ("sexp serialization failed to round-trip"
        (original       (23:59:29.999999 America/New_York))
        (sexp           (23:59:29.999999 America/New_York))
        (sexp_roundtrip (23:59:29.999999 America/New_York)))
      ((sexp (00:00:00.000001 America/New_York))
       (bin_io "\141\237\181\160\247\198\176>\016America/New_York"))
      ((sexp (00:00:00.001000 America/New_York))
       (bin_io "\252\169\241\210MbP?\016America/New_York"))
      ((sexp (00:00:01.000000 America/New_York))
       (bin_io "\000\000\000\000\000\000\240?\016America/New_York"))
      ((sexp (00:01:00.000000 America/New_York))
       (bin_io "\000\000\000\000\000\000N@\016America/New_York"))
      ((sexp (01:00:00.000000 America/New_York))
       (bin_io "\000\000\000\000\000 \172@\016America/New_York"))
      ((sexp (24:00:00.000000 America/New_York))
       (bin_io "\000\000\000\000\000\024\245@\016America/New_York"))
      ((sexp (05:00:38.770479 America/New_York))
       (bin_io "\208&\135O\177\157\209@\016America/New_York"))
      ((sexp (20:30:29.782106 America/New_York))
       (bin_io "\193\148\129\131\\\006\242@\016America/New_York"))
      (* require-failed: lib/time_float_unix/test/test_time_unix.ml:LINE:COL. *)
      ("sexp serialization failed to round-trip"
        (original       (20:30:29.782106 America/New_York))
        (sexp           (20:30:29.782106 America/New_York))
        (sexp_roundtrip (20:30:29.782106 America/New_York)))
      ((sexp (06:18:35.327424 America/New_York))
       (bin_io "\252\202\131\244\212.\214@\016America/New_York"))
      (* require-failed: lib/time_float_unix/test/test_time_unix.ml:LINE:COL. *)
      ("sexp serialization failed to round-trip"
        (original       (06:18:35.327424 America/New_York))
        (sexp           (06:18:35.327424 America/New_York))
        (sexp_roundtrip (06:18:35.327424 America/New_York)))
      ((sexp (18:12:08.192141 America/New_York))
       (bin_io "\231\225\004&\006\255\239@\016America/New_York"))
      ((sexp (06:40:20.750241 America/New_York))
       (bin_io "\200\211\242\0030u\215@\016America/New_York"))
      |}]
  ;;
end

module%test [@name "Time.Stable.Zone"] _ = struct
  let examples =
    [ Zone.utc; Zone.find_exn "hkg"; Zone.find_exn "ldn"; Zone.find_exn "nyc" ]
  ;;

  let%expect_test "V1" =
    print_and_check_stable_type (module Time.Stable.Zone.V1) examples;
    [%expect
      {|
      (bin_shape_digest d9a8da25d5656b016fb4dbdc2e4197fb)
      ((sexp   UTC)
       (bin_io "\003UTC"))
      ((sexp   Asia/Hong_Kong)
       (bin_io "\014Asia/Hong_Kong"))
      ((sexp   Europe/London)
       (bin_io "\rEurope/London"))
      ((sexp   America/New_York)
       (bin_io "\016America/New_York"))
      |}]
  ;;
end

let%expect_test "Span.randomize" =
  let span = Span.of_sec 1. in
  let upper_bound = Span.of_sec 1.3 in
  let lower_bound = Span.of_sec 0.7 in
  let percent = Percent.of_mult 0.3 in
  let rec loop ~count ~trials =
    let open Int.O in
    if count >= trials
    then print_s [%message "succeeded" (count : int)]
    else (
      let rand = Span.randomize span ~percent in
      if Span.( < ) rand lower_bound || Span.( > ) rand upper_bound
      then
        print_cr
          [%message
            "out of bounds"
              (percent : Percent.t)
              (rand : Span.t)
              (lower_bound : Span.t)
              (upper_bound : Span.t)]
      else loop ~count:(count + 1) ~trials)
  in
  loop ~count:0 ~trials:1_000;
  [%expect {| (succeeded (count 1000)) |}]
;;

let%expect_test "Span.to_short_string" =
  let examples =
    let magnitudes = [ 1.; Float.pi; 10.6 ] in
    let pos_examples =
      List.concat_map magnitudes ~f:(fun magnitude ->
        List.map Unit_of_time.all ~f:(fun unit_of_time ->
          Span.scale (Span.of_unit_of_time unit_of_time) magnitude))
    in
    let signed_examples =
      List.concat_map pos_examples ~f:(fun span -> [ span; Span.neg span ])
    in
    Span.zero :: signed_examples
  in
  let alist = List.map examples ~f:(fun span -> span, Span.to_short_string span) in
  print_s [%sexp (alist : (Span.t * string) list)];
  [%expect
    {|
    ((0s                           0ns)
     (1ns                          1ns)
     (-1ns                         -1ns)
     (1us                          1us)
     (-1us                         -1us)
     (1ms                          1ms)
     (-1ms                         -1ms)
     (1s                           1s)
     (-1s                          -1s)
     (1m                           1m)
     (-1m                          -1m)
     (1h                           1h)
     (-1h                          -1h)
     (1d                           1d)
     (-1d                          -1d)
     (3.1415926535897931ns         3ns)
     (-3.1415926535897931ns        -3ns)
     (3.1415926535897927us4e-13ns  3.1us)
     (-3.1415926535897927us4e-13ns -3.1us)
     (3.1415926535897931ms         3.1ms)
     (-3.1415926535897931ms        -3.1ms)
     (3.1415926535897931s          3.1s)
     (-3.1415926535897931s         -3.1s)
     (3m8.49555921538757s          3.1m)
     (-3m8.49555921538757s         -3.1m)
     (3h8m29.733552923255s         3.1h)
     (-3h8m29.733552923255s        -3.1h)
     (3d3h23m53.60527015815s       3.1d)
     (-3d3h23m53.60527015815s      -3.1d)
     (10.600000000000001ns         11ns)
     (-10.600000000000001ns        -11ns)
     (10.599999999999998us         10us)
     (-10.599999999999998us        -10us)
     (10.6ms                       10ms)
     (-10.6ms                      -10ms)
     (10.6s                        10s)
     (-10.6s                       -10s)
     (10m36s                       10m)
     (-10m36s                      -10m)
     (10h36m                       10h)
     (-10h36m                      -10h)
     (10d14h24m                    10d)
     (-10d14h24m                   -10d))
    |}]
;;

let%expect_test "times with implicit zones" =
  let test f = show_raise (fun () -> print_endline (Time.to_string (f ()))) in
  test (fun () ->
    Time.Stable.With_utc_sexp.V2.t_of_sexp (Sexp.of_string "(2013-10-07 09:30)"));
  [%expect
    {|
    2013-10-07 05:30:00.000000-04:00
    "did not raise"
    |}];
  test (fun () -> Time.Stable.V1.t_of_sexp (Sexp.of_string "(2013-10-07 09:30)"));
  [%expect
    {|
    2013-10-07 09:30:00.000000-04:00
    "did not raise"
    |}];
  test (fun () -> Time.t_of_sexp (Sexp.Atom "2013-10-07 09:30"));
  [%expect
    {|
    2013-10-07 09:30:00.000000-04:00
    "did not raise"
    |}];
  test (fun () -> Time.of_string "2013-10-07 09:30");
  [%expect
    {|
    2013-10-07 09:30:00.000000-04:00
    "did not raise"
    |}]
;;

let%test_unit "ofday_zoned conversion consistency" =
  let quickcheck_generator =
    Int64.gen_uniform_incl (-10_000_000_000_000_000L) 10_000_000_000_000_000L
    |> Quickcheck.Generator.map ~f:(fun int64 ->
      Time.of_span_since_epoch (Span.of_us (Int64.to_float int64)))
  in
  let utc = Zone.utc in
  let nyc = Zone.find_exn "America/New_York" in
  let hkg = Zone.find_exn "Asia/Hong_Kong" in
  require_does_not_raise (fun () ->
    Quickcheck.test quickcheck_generator ~trials:100 ~f:(fun time ->
      let date_utc = Time.to_date time ~zone:utc in
      let date_nyc = Time.to_date time ~zone:nyc in
      let date_hkg = Time.to_date time ~zone:hkg in
      let ofday_utc = Time.to_ofday time ~zone:utc in
      let ofday_nyc = Time.to_ofday time ~zone:nyc in
      let ofday_hkg = Time.to_ofday time ~zone:hkg in
      let ofday_zoned_utc = Time.to_ofday_zoned time ~zone:utc in
      let ofday_zoned_nyc = Time.to_ofday_zoned time ~zone:nyc in
      let ofday_zoned_hkg = Time.to_ofday_zoned time ~zone:hkg in
      let date_utc2, ofday_zoned_utc2 = Time.to_date_ofday_zoned time ~zone:utc in
      let date_nyc2, ofday_zoned_nyc2 = Time.to_date_ofday_zoned time ~zone:nyc in
      let date_hkg2, ofday_zoned_hkg2 = Time.to_date_ofday_zoned time ~zone:hkg in
      let time_utc = Time.of_date_ofday_zoned date_utc ofday_zoned_utc in
      let time_nyc = Time.of_date_ofday_zoned date_nyc ofday_zoned_nyc in
      let time_hkg = Time.of_date_ofday_zoned date_hkg ofday_zoned_hkg in
      assert (Zone.( = ) (Ofday.Zoned.zone ofday_zoned_utc) utc);
      assert (Zone.( = ) (Ofday.Zoned.zone ofday_zoned_nyc) nyc);
      assert (Zone.( = ) (Ofday.Zoned.zone ofday_zoned_hkg) hkg);
      assert (Ofday.( = ) (Ofday.Zoned.ofday ofday_zoned_utc) ofday_utc);
      assert (Ofday.( = ) (Ofday.Zoned.ofday ofday_zoned_nyc) ofday_nyc);
      assert (Ofday.( = ) (Ofday.Zoned.ofday ofday_zoned_hkg) ofday_hkg);
      assert (Ofday.( = ) (Ofday.Zoned.ofday ofday_zoned_utc2) ofday_utc);
      assert (Ofday.( = ) (Ofday.Zoned.ofday ofday_zoned_nyc2) ofday_nyc);
      assert (Ofday.( = ) (Ofday.Zoned.ofday ofday_zoned_hkg2) ofday_hkg);
      assert (Date.( = ) date_utc2 date_utc);
      assert (Date.( = ) date_nyc2 date_nyc);
      assert (Date.( = ) date_hkg2 date_hkg);
      assert (Time.( = ) time_utc time);
      assert (Time.( = ) time_nyc time);
      assert (Time.( = ) time_hkg time)))
;;

let%expect_test "sexp grammar" =
  require_ok (Sexp_grammar_validation.validate_grammar (module Time_float_unix));
  [%expect
    {|
    (Union
     (String
      (List (Cons String (Cons String Empty)))
      (List (Cons String (Cons String (Cons String Empty))))))
    |}]
;;
