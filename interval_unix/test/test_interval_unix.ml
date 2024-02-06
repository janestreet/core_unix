open! Core
open! Expect_test_helpers_core
module Time = Time_float_unix
module Interval = Interval_lib.Interval

let%test_module "Interval.V1.Time" =
  (module struct
    module Arg = struct
      include Interval_unix.Stable.V1.Time

      let equal = [%compare.equal: t]
      let zone = force Jane_timezone.nyc

      let tests =
        let t1 =
          Time.of_date_ofday
            ~zone
            (Date.create_exn ~y:2013 ~m:Month.Aug ~d:6)
            (Time.Ofday.create ~hr:7 ~min:30 ~sec:7 ~ms:12 ~us:5 ())
        in
        let t2 =
          Time.of_date_ofday
            ~zone
            (Date.create_exn ~y:2014 ~m:Month.Sep ~d:8)
            (Time.Ofday.create ~hr:10 ~min:10 ~sec:0 ~ms:22 ~us:0 ())
        in
        Interval_lib_test.Test_interval.make_stable_unit_tests_v1
          ~coerce:Interval.Stable.V1.Private.to_time
          ~non_empty:
            [ ( (t1, t2)
              , "((2013-08-06 07:30:07.012005-04:00) (2014-09-08 10:10:00.022000-04:00))"
              , "\000\177\196\192\1437\128\212Ash\001.n\003\213A" )
            ]
      ;;
    end

    (* Bypass sexp serialization tests because [Time.sexp_of_t] gives different
       results depending on the local zone. *)
    include Stable_unit_test.Make_sexp_deserialization_test (Arg)
    include Stable_unit_test.Make_bin_io_test (Arg)
  end)
;;
