open! Core
open! Import

type t =
  | AB of int * int
  | CD of int * string
  | EFG of string * int * string
[@@deriving sexp_of]

let%expect_test "basic" =
  let param =
    let%map_open.Command data =
      choose_one_non_optional
        ~if_nothing_chosen:Return_none
        [ (let%map_open.Command a = flag "-a" (required int) ~doc:"INT a"
           and b = flag "-b" (required int) ~doc:"INT b" in
           AB (a, b))
        ]
    in
    data
  in
  let on_success data = print_s [%message (data : t option)] in
  let test = (Command_test_helpers.parse_command_line param |> unstage) ~on_success in
  test [ "-a"; "1"; "-b"; "123" ];
  [%expect {| (data ((AB 1 123))) |}];
  test [];
  [%expect {| (data ()) |}];
  require_does_raise [%here] (fun () -> test [ "-a"; "1" ]);
  [%expect
    {|
    Error parsing command line:

      Not all flags in group "-a,-b" are given: missing required flag: -b

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}];
  require_does_raise [%here] (fun () -> test [ "-a"; "not-an-int" ]);
  [%expect
    {|
    Error parsing command line:

      failed to parse -a value "not-an-int".
      (Failure "Int.of_string: \"not-an-int\"")

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}];
  test [ "-help" ];
  [%expect
    {|
    CMD SUMMARY

      CMD

    === flags ===

      [-a INT]                   . a [requires: "-b"]
      [-b INT]                   . b [requires: "-a"]
      [-build-info]              . print info about this build and exit
      [-version]                 . print the version of this build and exit
      [-help], -?                . print this help text and exit

    (command.ml.Exit_called (status 0))
    |}]
;;

let%expect_test "interaction with no_arg" =
  let param =
    let%map_open.Command data =
      choose_one_non_optional
        ~if_nothing_chosen:Return_none
        [ (let%map_open.Command a = flag "-a" (required int) ~doc:"INT a"
           and b = flag "-b" no_arg ~doc:"b"
           and c = flag "-c" (required int) ~doc:"INT c" in
           a, b, c)
        ]
    in
    data
  in
  let on_success data = print_s [%message (data : (int * bool * int) option)] in
  let test = (Command_test_helpers.parse_command_line param |> unstage) ~on_success in
  test [ "-a"; "1"; "-b"; "-c"; "10" ];
  [%expect {| (data ((1 true 10))) |}];
  test [ "-a"; "1"; "-c"; "100" ];
  [%expect {| (data ((1 false 100))) |}];
  test [];
  [%expect {| (data ()) |}];
  require_does_raise [%here] (fun () -> test [ "-a"; "10" ]);
  [%expect
    {|
    Error parsing command line:

      Not all flags in group "-a,-c" are given: missing required flag: -c

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}];
  require_does_raise [%here] (fun () -> test [ "-b" ]);
  [%expect
    {|
    Error parsing command line:

      Not all flags in group "-a,-c" are given: missing required flag: -a

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}];
  test [ "-help" ];
  [%expect
    {|
    CMD SUMMARY

      CMD

    === flags ===

      [-a INT]                   . a [requires: "-c"]
      [-b]                       . b [requires: "-a,-c"]
      [-c INT]                   . c [requires: "-a"]
      [-build-info]              . print info about this build and exit
      [-version]                 . print the version of this build and exit
      [-help], -?                . print this help text and exit

    (command.ml.Exit_called (status 0))
    |}]
;;

let%expect_test "with choose one" =
  let param =
    let%map_open.Command data =
      choose_one_non_optional
        ~if_nothing_chosen:Raise
        [ (let%map_open.Command a = flag "-a" (required int) ~doc:"INT a"
           and b = flag "-b" (required int) ~doc:"INT b" in
           AB (a, b))
        ; (let%map_open.Command c = flag "-c" (required int) ~doc:"INT c"
           and d = flag "-d" (required string) ~doc:"STRING d" in
           CD (c, d))
        ; (let%map_open.Command e = flag "-e" (required string) ~doc:"INT e"
           and f = flag "-f" (required int) ~doc:"INT f"
           and g = flag "-g" (required string) ~doc:"STRING g" in
           EFG (e, f, g))
        ]
    in
    data
  in
  let on_success data = print_s [%message (data : t)] in
  let test = (Command_test_helpers.parse_command_line param |> unstage) ~on_success in
  test [ "-a"; "1"; "-b"; "123" ];
  [%expect {| (data (AB 1 123)) |}];
  test [ "-c"; "3"; "-d"; "world" ];
  [%expect {| (data (CD 3 world)) |}];
  test [ "-e"; "hello"; "-f"; "456"; "-g"; "world" ];
  [%expect {| (data (EFG hello 456 world)) |}];
  require_does_raise [%here] (fun () -> test [ "-a"; "1" ]);
  [%expect
    {|
    Error parsing command line:

      Not all flags in group "-a,-b" are given: missing required flag: -b

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}];
  require_does_raise [%here] (fun () -> test [ "-g"; "abcde" ]);
  [%expect
    {|
    Error parsing command line:

      Not all flags in group "-e,-f,-g" are given: missing required flag: -e

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}];
  require_does_raise [%here] (fun () -> test []);
  [%expect
    {|
    Error parsing command line:

      Must pass one of these:
        -a,-b
        -c,-d
        -e,-f,-g

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}];
  require_does_raise [%here] (fun () ->
    test [ "-a"; "1"; "-b"; "123"; "-c"; "3"; "-d"; "world" ]);
  [%expect
    {|
    Error parsing command line:

      Cannot pass more than one of these:
        -c,-d
        -a,-b

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}];
  require_does_raise [%here] (fun () -> test [ "-a"; "1"; "-b"; "123"; "-c"; "3" ]);
  [%expect
    {|
    Error parsing command line:

      Cannot pass more than one of these:
        -c,-d
        -a,-b

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}];
  require_does_raise [%here] (fun () -> test [ "-a"; "1"; "-b"; "123"; "-g"; "abcde" ]);
  [%expect
    {|
    Error parsing command line:

      Cannot pass more than one of these:
        -e,-f,-g
        -a,-b

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}];
  require_does_raise [%here] (fun () -> test [ "-a"; "1"; "-c"; "3" ]);
  [%expect
    {|
    Error parsing command line:

      Must pass one of these:
        -a,-b
        -c,-d
        -e,-f,-g

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}];
  require_does_raise [%here] (fun () -> test [ "-a"; "1"; "-g"; "3" ]);
  [%expect
    {|
    Error parsing command line:

      Must pass one of these:
        -a,-b
        -c,-d
        -e,-f,-g

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}];
  test [ "-help" ];
  [%expect
    {|
    CMD SUMMARY

      CMD

    === flags ===

      [-a INT]                   . a [requires: "-b"]
      [-b INT]                   . b [requires: "-a"]
      [-c INT]                   . c [requires: "-d"]
      [-d STRING]                . d [requires: "-c"]
      [-e INT]                   . e [requires: "-f,-g"]
      [-f INT]                   . f [requires: "-e,-g"]
      [-g STRING]                . g [requires: "-e,-f"]
      [-build-info]              . print info about this build and exit
      [-version]                 . print the version of this build and exit
      [-help], -?                . print this help text and exit

    (command.ml.Exit_called (status 0))
    |}]
;;

let%expect_test "listed flag" =
  let param =
    let%map_open.Command data =
      choose_one_non_optional
        ~if_nothing_chosen:Raise
        [ (let%map_open.Command a = flag "-a" (listed int) ~doc:"INT a"
           and b = flag "-b" (required int) ~doc:"INT b" in
           a, b)
        ]
    in
    data
  in
  let on_success data = print_s [%message (data : int list * int)] in
  let test = (Command_test_helpers.parse_command_line param |> unstage) ~on_success in
  test [ "-help" ];
  test [ "-a"; "1"; "-a"; "2"; "-b"; "3" ];
  [%expect
    {|
    CMD SUMMARY

      CMD

    === flags ===

      [-a INT] ...               . a [requires: "-b"]
      [-b INT]                   . b
      [-build-info]              . print info about this build and exit
      [-version]                 . print the version of this build and exit
      [-help], -?                . print this help text and exit

    (command.ml.Exit_called (status 0))
    (data ((1 2) 3))
    |}]
;;

let%expect_test "[return] is disallowed as a standalone item given to choose_one" =
  require_does_raise [%here] (fun () ->
    Command.Param.choose_one_non_optional
      ~if_nothing_chosen:Raise
      [ Command.Param.return 8 ]);
  [%expect {| "[choose_one] expects choices to read command-line arguments." |}];
  ()
;;

(* [listed _] is an example of an "optional" param, that is a param which can succeed
   without consuming any flags. If that happens, [choose_one_*] does *not* consider this
   as a "true" success, instead demanding that some flag is given.

   Same for [return _], but [return _] is already special-cased. *)
let%expect_test "listed flag as a standalone item given to choose_one" =
  let param =
    let open Command.Param in
    choose_one_non_optional
      ~if_nothing_chosen:Raise
      [ (* the [listed] branches are only taken when their list is non-empty *)
        flag "-a" (listed int) ~doc:"INT a"
      ; flag "-b" (listed int) ~doc:"INT b"
      ]
  in
  let on_success data = print_s [%message (data : int list)] in
  let test = (Command_test_helpers.parse_command_line param |> unstage) ~on_success in
  test [ "-help" ];
  require_does_raise [%here] (fun () -> test [ "-a"; "1"; "-a"; "2"; "-b"; "3" ]);
  [%expect
    {|
    CMD SUMMARY

      CMD

    === flags ===

      [-a INT] ...               . a
      [-b INT] ...               . b
      [-build-info]              . print info about this build and exit
      [-version]                 . print the version of this build and exit
      [-help], -?                . print this help text and exit

    (command.ml.Exit_called (status 0))
    Error parsing command line:

      Cannot pass more than one of these:
        -b
        -a

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}];
  test [ "-a"; "1" ];
  [%expect {| (data (1)) |}];
  test [ "-b"; "1" ];
  [%expect "(data (1))"];
  require_does_raise [%here] (fun () -> test []);
  [%expect
    {|
    Error parsing command line:

      Must pass one of these:
        -a
        -b

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}]
;;

let%expect_test "nested" =
  let param =
    let%map_open.Command data =
      choose_one_non_optional
        ~if_nothing_chosen:Return_none
        [ (let%map_open.Command a = flag "-a" (required int) ~doc:"INT a"
           and b = flag "-b" (required int) ~doc:"INT b"
           and c =
             choose_one
               ~if_nothing_chosen:Raise
               [ flag "-yes" (no_arg_some "yes") ~doc:"yes"
               ; flag "-no" (no_arg_some "no") ~doc:"no"
               ]
           in
           a, b, c)
        ]
    in
    data
  in
  let on_success data = print_s [%message (data : (int * int * string) option)] in
  let test = (Command_test_helpers.parse_command_line param |> unstage) ~on_success in
  test [ "-a"; "1"; "-b"; "123"; "-yes" ];
  [%expect {| (data ((1 123 yes))) |}];
  test [ "-a"; "1"; "-b"; "123"; "-no" ];
  [%expect {| (data ((1 123 no))) |}];
  test [];
  [%expect {| (data ()) |}];
  test [ "--help" ];
  [%expect
    {|
    CMD SUMMARY

      CMD

    === flags ===

      [-a INT]                   . a [requires: "-b"]
      [-b INT]                   . b [requires: "-a"]
      [-no]                      . no [requires: "-a,-b"]
      [-yes]                     . yes [requires: "-a,-b"]
      [-build-info]              . print info about this build and exit
      [-version]                 . print the version of this build and exit
      [-help], -?                . print this help text and exit

    (command.ml.Exit_called (status 0))
    |}];
  require_does_raise [%here] (fun () -> test [ "-a"; "1"; "-b"; "2"; "-yes"; "-no" ]);
  [%expect
    {|
    Error parsing command line:

      Cannot pass more than one of these:
        -yes
        -no

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}];
  require_does_raise [%here] (fun () -> test [ "-a"; "1"; "-b"; "2" ]);
  [%expect
    {|
    Error parsing command line:

      Not all flags in group "-a,-b" are given: Must pass one of these:
        -no
        -yes

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}]
;;

let%expect_test "with anons" =
  let module R = struct
    type t =
      | A of int
      | B of bool * string list * string list option
    [@@deriving sexp_of]
  end
  in
  let param =
    let%map_open.Command data =
      choose_one_non_optional
        ~if_nothing_chosen:Raise
        [ (let%map_open.Command a = flag "-a" (required int) ~doc:"INT a" in
           R.A a)
        ; (let%map_open.Command flag_is_given =
             (* This flag is not technically needed, but people may get confused if we
                just start searching for errors. *)
             flag
               "-search-in-errors"
               no_arg
               ~doc:"search failed build matching the search-engine like QUERY"
           and qs1 = anon (sequence ("QUERY" %: string))
           and qs2 = flag "--" escape ~doc:"QUERIES" in
           R.B (flag_is_given, qs1, qs2))
        ]
    in
    data
  in
  let on_success data = print_s [%message (data : R.t)] in
  let test = (Command_test_helpers.parse_command_line param |> unstage) ~on_success in
  test [ "--help" ];
  [%expect
    {|
    CMD SUMMARY

      CMD [QUERY ...]

    === flags ===

      [--]                       . QUERIES
      [-a INT]                   . a
      [-search-in-errors]        . search failed build matching the search-engine
                                   like QUERY
      [-build-info]              . print info about this build and exit
      [-version]                 . print the version of this build and exit
      [-help], -?                . print this help text and exit

    (command.ml.Exit_called (status 0))
    |}];
  test [ "-a"; "123" ];
  [%expect {| (data (A 123)) |}];
  test [ "-search-in-errors"; "123"; "abcde" ];
  [%expect {| (data (B true (123 abcde) ())) |}]
;;

let%expect_test "with regular flags" =
  let param =
    let%map_open.Command data =
      choose_one_non_optional
        ~if_nothing_chosen:Raise
        [ flag "-a" (required int) ~doc:"INT a"; flag "-b" (required int) ~doc:"INT b" ]
    in
    data
  in
  let on_success data = print_s [%message (data : int)] in
  let test = (Command_test_helpers.parse_command_line param |> unstage) ~on_success in
  test [ "-a"; "123" ];
  [%expect {| (data 123) |}];
  test [ "--help" ];
  [%expect
    {|
    CMD SUMMARY

      CMD

    === flags ===

      [-a INT]                   . a
      [-b INT]                   . b
      [-build-info]              . print info about this build and exit
      [-version]                 . print the version of this build and exit
      [-help], -?                . print this help text and exit

    (command.ml.Exit_called (status 0))
    |}];
  require_does_raise [%here] (fun () -> test []);
  [%expect
    {|
    Error parsing command line:

      Must pass one of these:
        -a
        -b

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}]
;;

let%expect_test "with maybe anon" =
  let param =
    let%map_open.Command data =
      choose_one_non_optional ~if_nothing_chosen:Raise [ anon (maybe ("ANON" %: int)) ]
    in
    data
  in
  let on_success data = print_s [%message (data : int option)] in
  let test = (Command_test_helpers.parse_command_line param |> unstage) ~on_success in
  test [ "1223" ];
  [%expect {| (data (1223)) |}]
;;

let%expect_test "with anon" =
  let param =
    let%map_open.Command data =
      choose_one_non_optional
        ~if_nothing_chosen:Return_none
        [ (let%map_open.Command flag_is_given =
             (* This flag is not technically needed, but people may get confused if we
                just start searching for errors. *)
             flag
               "-search-in-errors"
               no_arg
               ~doc:"search failed build matching the search-engine like QUERY"
           and qs1 = anon ("ANON" %: int) in
           flag_is_given, qs1)
        ]
    in
    data
  in
  let on_success data = print_s [%message (data : (bool * int) option)] in
  let test = (Command_test_helpers.parse_command_line param |> unstage) ~on_success in
  test [];
  [%expect {| (data ()) |}];
  test [ "--help" ];
  [%expect
    {|
    CMD SUMMARY

      CMD [ANON]

    === flags ===

      [-search-in-errors]        . search failed build matching the search-engine
                                   like QUERY
      [-build-info]              . print info about this build and exit
      [-version]                 . print the version of this build and exit
      [-help], -?                . print this help text and exit

    (command.ml.Exit_called (status 0))
    |}]
;;

let%expect_test "with anon" =
  let param =
    let%map_open.Command data =
      choose_one_non_optional
        ~if_nothing_chosen:Raise
        [ (let%map_open.Command flag_is_given =
             (* This flag is not technically needed, but people may get confused if we
                just start searching for errors. *)
             flag
               "-search-in-errors"
               no_arg
               ~doc:"search failed build matching the search-engine like QUERY"
           and qs1 = anon ("ANON" %: int) in
           flag_is_given, qs1)
        ]
    in
    data
  in
  let on_success data = print_s [%message (data : bool * int)] in
  let test = (Command_test_helpers.parse_command_line param |> unstage) ~on_success in
  require_does_raise [%here] (fun () -> test [ "-search-in-errors" ]);
  [%expect
    {|
    Error parsing command line:

      Not all flags in group "-search-in-errors,ANON" are given: missing anonymous argument: ANON

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}]
;;

let%expect_test "no_arg_required" =
  let param =
    let%map_open.Command data =
      choose_one_non_optional
        ~if_nothing_chosen:Return_none
        [ (let%map_open.Command a =
             flag "-must-pass-me" (no_arg_required ()) ~doc:" must pass this flag"
           and b = flag "-optional-flag" no_arg ~doc:" an optional flag" in
           a, b)
        ]
    in
    data
  in
  let on_success data = print_s [%message (data : (unit * bool) option)] in
  let test = (Command_test_helpers.parse_command_line param |> unstage) ~on_success in
  test [ "-must-pass-me"; "-optional-flag" ];
  [%expect {| (data ((() true))) |}];
  test [ "-must-pass-me" ];
  [%expect {| (data ((() false))) |}];
  test [];
  [%expect {| (data ()) |}];
  require_does_raise [%here] (fun () -> test [ "-optional-flag" ]);
  [%expect
    {|
    Error parsing command line:

      Not all flags in group "-must-pass-me" are given: missing required flag: -must-pass-me

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}];
  test [ "-help" ];
  [%expect
    {|
    CMD SUMMARY

      CMD

    === flags ===

      [-must-pass-me]            . must pass this flag
      [-optional-flag]           . an optional flag [requires: "-must-pass-me"]
      [-build-info]              . print info about this build and exit
      [-version]                 . print the version of this build and exit
      [-help], -?                . print this help text and exit

    (command.ml.Exit_called (status 0))
    |}]
;;

let%expect_test "choose_one with an optional that can be set from an environment_variable"
  =
  let maybe_set_via_environment_variable_param environment_variable =
    [%map_open.Command
      match
        flag
          "sometimes-set-via-env-var"
          (Command.Param.optional Command.Param.string)
          ~doc:"ignored"
      with
      | Some _ as some -> some
      | None -> environment_variable]
  in
  (* Combining this parameter with another option in [choose_one]. *)
  let choose_one_param environment_variable =
    let some_other_param =
      Command.Param.flag
        "some-other-option"
        (Command.Param.optional Command.Param.string)
        ~doc:"ignored"
    in
    Command.Param.choose_one
      ~if_nothing_chosen:Raise
      [ maybe_set_via_environment_variable_param environment_variable; some_other_param ]
  in
  let test_choose_one environment_variable =
    let on_success data = print_s [%message (data : string)] in
    (Command_test_helpers.parse_command_line (choose_one_param environment_variable)
     |> unstage)
      ~on_success
  in
  (* We expect [choose_one] to succeed in this case because the optional parameter
     is set via the environment variable. *)
  test_choose_one (Some "value") [];
  [%expect {| (data value) |}];
  (* If the environment_variable is set AND "the other option" is set, we expect
     choose_one to fail. *)
  require_does_raise [%here] (fun () ->
    test_choose_one (Some "value") [ "-some-other-option"; "value" ]);
  [%expect
    {|
    Error parsing command line:

      Cannot pass more than one of these:
        -sometimes-set-via-env-var
        -some-other-option

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}]
;;

let%expect_test "choose_one with an optional that can be suppressed from an \
                 environment_variable"
  =
  let maybe_set_via_environment_variable_param environment_variable =
    [%map_open.Command
      match
        flag
          "suppressed-via-env-var"
          (Command.Param.optional Command.Param.string)
          ~doc:"ignored"
      with
      | _ when environment_variable -> None
      | x -> x]
  in
  (* Combining this parameter with another option in [choose_one]. *)
  let choose_one_param environment_variable =
    let some_other_param =
      Command.Param.flag
        "other-option"
        (Command.Param.optional Command.Param.string)
        ~doc:"ignored"
    in
    Command.Param.choose_one
      ~if_nothing_chosen:Raise
      [ maybe_set_via_environment_variable_param environment_variable; some_other_param ]
  in
  let test_choose_one environment_variable =
    let on_success data = print_s [%message (data : string)] in
    (Command_test_helpers.parse_command_line (choose_one_param environment_variable)
     |> unstage)
      ~on_success
  in
  (* We would expect [choose_one] to succeed in this case because the optional parameter
     is suppressed. *)
  test_choose_one
    true
    [ "-suppressed-via-env-var"; "value"; "-other-option"; "other-value" ];
  [%expect {| (data other-value) |}];
  (* We expect [choose_one] to say there's nothing to choose from. *)
  require_does_raise [%here] (fun () ->
    test_choose_one true [ "-suppressed-via-env-var"; "value" ]);
  [%expect
    {|
    Error parsing command line:

      Must pass one of these:
        -other-option
        -suppressed-via-env-var

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    |}]
;;
