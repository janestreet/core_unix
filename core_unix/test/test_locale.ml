open! Core
open! Import
module Locale = Unix.Locale

let print_global_locale () =
  let l = Locale.Expert.global () in
  print_endline (Locale.to_string_hum l);
  Locale.Expert.free l
;;

let print_current_locale () =
  let l = Locale.Expert.current () in
  print_endline (Locale.to_string_hum l);
  Locale.Expert.free l
;;

let%expect_test "Locale.Expert.global" =
  let locale = Locale.Expert.global () in
  print_endline (Locale.to_string_hum locale);
  [%expect {| C |}];
  Locale.Expert.set_global "de_DE.UTF-8";
  print_endline (Locale.to_string_hum locale);
  [%expect {| C |}];
  Locale.Expert.free locale;
  print_global_locale ();
  [%expect {| de_DE.UTF-8 |}];
  Locale.Expert.set_global Locale.Name.posix
;;

let%expect_test "Locale.Expert.current" =
  let locale = Locale.Expert.current () in
  print_endline (Locale.to_string_hum locale);
  [%expect {| C |}];
  let locale' = Locale.Expert.create "de_DE.UTF-8" in
  let old = Locale.Expert.set_current (Some locale') in
  print_global_locale ();
  [%expect {| C |}];
  Locale.Expert.set_global "es_ES.UTF-8";
  print_endline (Locale.to_string_hum locale);
  [%expect {| C |}];
  Locale.Expert.free locale;
  let locale = Locale.Expert.current () in
  print_endline (Locale.to_string_hum locale);
  [%expect {| de_DE.UTF-8 |}];
  ignore (Locale.Expert.set_current old : Locale.t option);
  Locale.Expert.free locale';
  print_endline (Locale.to_string_hum locale);
  [%expect {| de_DE.UTF-8 |}];
  Locale.Expert.free locale;
  let locale = Locale.Expert.current () in
  print_endline (Locale.to_string_hum locale);
  [%expect {| es_ES.UTF-8 |}];
  Locale.Expert.free locale;
  Locale.Expert.set_global Locale.Name.posix
;;

let%expect_test "Locale.Expert.posix" =
  let locale = Locale.Expert.posix () in
  print_endline (Locale.to_string_hum locale);
  [%expect {| C |}];
  Locale.Expert.free locale
;;

let%expect_test "Locale.Expert.native" =
  let locale = Locale.Expert.native () in
  print_endline (Locale.to_string_hum locale);
  (* Tests run in a clean environment, so no locale environment variables will be set. *)
  [%expect {| C |}];
  Unix.putenv ~key:"LC_ALL" ~data:"de_DE.UTF-8";
  print_endline (Locale.to_string_hum locale);
  (* Changing the environment does not affect the existing [Locale.t] or its string
     conversion. *)
  [%expect {| C |}];
  Locale.Expert.free locale;
  let locale = Locale.Expert.native () in
  print_endline (Locale.to_string_hum locale);
  (* ...but does affect a newly constructed one. *)
  [%expect {| de_DE.UTF-8 |}];
  Unix.unsetenv "LC_ALL";
  print_endline (Locale.to_string_hum locale);
  (* Changing the environment back does not affect it. *)
  [%expect {| de_DE.UTF-8 |}];
  Locale.Expert.free locale
;;

let%expect_test "Locale.Expert.create" =
  let base =
    Locale.Expert.create
      ~category_mask:Locale.Category_set.(collate + monetary)
      "de_DE.UTF-8"
  in
  print_endline (Locale.to_string_hum base);
  [%expect
    {| Ctype: C, Collate: de_DE.UTF-8, Messages: C, Monetary: de_DE.UTF-8, Numeric: C, Time: C |}];
  let locale =
    Locale.Expert.create
      ~base
      ~category_mask:Locale.Category_set.(messages + monetary + numeric)
      "es_ES.UTF-8"
  in
  Locale.Expert.free base;
  print_endline (Locale.to_string_hum locale);
  [%expect
    {| Ctype: C, Collate: de_DE.UTF-8, Messages: es_ES.UTF-8, Monetary: es_ES.UTF-8, Numeric: es_ES.UTF-8, Time: C |}];
  Locale.Expert.free locale
;;

let%expect_test "Locale.Expert.create_multi" =
  let base =
    Locale.Expert.create_multi
      Locale.Category_set.
        [ collate + monetary, "de_DE.UTF-8"; collate + numeric, "es_ES.UTF-8" ]
  in
  print_endline (Locale.to_string_hum base);
  [%expect
    {| Ctype: C, Collate: es_ES.UTF-8, Messages: C, Monetary: de_DE.UTF-8, Numeric: es_ES.UTF-8, Time: C |}];
  let locale =
    Locale.Expert.create_multi
      ~base
      Locale.Category_set.
        [ ctype + monetary, "en_US.UTF-8"; messages + time, "en_GB.UTF-8"; empty, "foo" ]
  in
  Locale.Expert.free base;
  print_endline (Locale.to_string_hum locale);
  [%expect
    {| Ctype: en_US.UTF-8, Collate: es_ES.UTF-8, Messages: en_GB.UTF-8, Monetary: en_US.UTF-8, Numeric: es_ES.UTF-8, Time: en_GB.UTF-8 |}];
  Locale.Expert.free locale
;;

let%expect_test "Locale.Expert.modify" =
  let locale =
    Locale.Expert.create
      ~category_mask:Locale.Category_set.(collate + monetary)
      "de_DE.UTF-8"
  in
  let locale =
    Locale.Expert.modify
      locale
      ~category_mask:Locale.Category_set.(messages + monetary + numeric)
      "es_ES.UTF-8"
  in
  print_endline (Locale.to_string_hum locale);
  [%expect
    {| Ctype: C, Collate: de_DE.UTF-8, Messages: es_ES.UTF-8, Monetary: es_ES.UTF-8, Numeric: es_ES.UTF-8, Time: C |}];
  Locale.Expert.free locale
;;

let%expect_test "Locale.Expert.modify_multi" =
  let locale =
    Locale.Expert.create
      ~category_mask:Locale.Category_set.(collate + monetary)
      "de_DE.UTF-8"
  in
  let locale =
    Locale.Expert.modify_multi
      locale
      Locale.Category_set.
        [ ctype + monetary, "en_US.UTF-8"; messages + time, "en_GB.UTF-8"; empty, "foo" ]
  in
  print_endline (Locale.to_string_hum locale);
  [%expect
    {| Ctype: en_US.UTF-8, Collate: de_DE.UTF-8, Messages: en_GB.UTF-8, Monetary: en_US.UTF-8, Numeric: C, Time: en_GB.UTF-8 |}];
  Locale.Expert.free locale
;;

let%expect_test "Locale.Expert.copy" =
  let locale =
    Locale.Expert.create
      ~category_mask:Locale.Category_set.(collate + monetary)
      "de_DE.UTF-8"
  in
  let copy = Locale.Expert.copy locale in
  [%test_result: bool] (Locale.equal copy locale) ~expect:false;
  print_endline (Locale.to_string_hum copy);
  [%expect
    {| Ctype: C, Collate: de_DE.UTF-8, Messages: C, Monetary: de_DE.UTF-8, Numeric: C, Time: C |}];
  let locale =
    Locale.Expert.modify
      locale
      ~category_mask:Locale.Category_set.(collate + numeric)
      "es_ES.UTF-8"
  in
  Locale.Expert.free locale;
  print_endline (Locale.to_string_hum copy);
  [%expect
    {| Ctype: C, Collate: de_DE.UTF-8, Messages: C, Monetary: de_DE.UTF-8, Numeric: C, Time: C |}]
;;

let%expect_test "Locale.with_current" =
  let locale = Locale.Expert.create "de_DE.UTF-8" in
  let save = Locale.Expert.set_current (Some locale) in
  let locale' = Locale.Expert.create "es_ES.UTF-8" in
  Locale.with_current (Some locale') (fun () ->
    [%test_result: bool]
      (Option.equal Locale.equal (Locale.get_current ()) (Some locale'))
      ~expect:true;
    print_current_locale ();
    [%expect {| es_ES.UTF-8 |}]);
  [%test_result: bool]
    (Option.equal Locale.equal (Locale.get_current ()) (Some locale))
    ~expect:true;
  print_current_locale ();
  [%expect {| de_DE.UTF-8 |}];
  Locale.Expert.free locale';
  ignore (Locale.Expert.set_current save : Locale.t option);
  Locale.Expert.free locale
;;

let%expect_test "Locale.posix" =
  let locale = force Locale.posix in
  print_endline (Locale.to_string_hum locale);
  [%expect {| C |}]
;;

let%expect_test "Locale.native" =
  Unix.putenv ~key:"LC_ALL" ~data:"de_DE.UTF-8";
  let locale = force Locale.native in
  Unix.unsetenv "LC_ALL";
  print_endline (Locale.to_string_hum locale);
  [%expect {| de_DE.UTF-8 |}]
;;
