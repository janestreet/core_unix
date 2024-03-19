open! Core
open! Import

let run ~add_validate_parsing_flag ~argv command =
  try Command_unix.run ~add_validate_parsing_flag ~argv command with
  | exn -> print_s [%message "raised" ~_:(exn : Exn.t)]
;;

module _ = struct
  module T = struct
    type nonrec t =
      | A
      | B
      | C
    [@@deriving enumerate, sexp_of]

    let to_string = function
      | A -> "alpha"
      | B -> "bravo"
      | C -> "charlie"
    ;;
  end

  include T

  let arg_type = Command.Arg_type.enumerated (module T)

  let command =
    Command.basic
      ~summary:"this displays the help text for enumerated arg types: stringing"
      (let%map_open.Command (_ : t) = anon ("T" %: arg_type)
       and (_ : t) = flag "f" (required arg_type) ~doc:"demo" in
       fun () -> print_endline "executing")
  ;;

  let%expect_test "Parsing correctly" =
    run
      ~add_validate_parsing_flag:true
      ~argv:[ "prog"; "-f"; "alpha"; "bravo"; "-validate-parsing" ]
      command;
    [%expect {| (command.ml.Exit_called (status 0)) |}]
  ;;

  let%expect_test "Parsing incorrectly" =
    run
      ~add_validate_parsing_flag:true
      ~argv:[ "prog"; "-f"; "alpha"; "unknown"; "-validate-parsing" ]
      command;
    [%expect
      {|
      Error parsing command line:

        failed to parse T value "unknown"
        (Failure "valid arguments: {alpha,bravo,charlie}")

      For usage information, run

        prog -help

      (raised (command.ml.Exit_called (status 1)))
      |}]
  ;;

  let%expect_test "Running normally" =
    run ~add_validate_parsing_flag:true ~argv:[ "prog"; "-f"; "alpha"; "bravo" ] command;
    [%expect {| executing |}]
  ;;

  let%expect_test "help text" =
    Command_unix.run ~add_validate_parsing_flag:true ~argv:[ "prog"; "-help" ] command;
    [%expect
      {|
      this displays the help text for enumerated arg types: stringing

        prog T

      === flags ===

        -f demo                    . (can be: alpha, bravo, charlie)
        [-validate-parsing]        . validate arguments are parsed correctly and exit
                                     immediately
        [-build-info]              . print info about this build and exit
        [-version]                 . print the version of this build and exit
        [-help], -?                . print this help text and exit

      (command.ml.Exit_called (status 0))
      |}]
  ;;
end

module _ = struct
  let command =
    Command.group
      ~summary:"This implements a basic command made of two different groups"
      [ ( "one"
        , Command.basic
            ~summary:"demo"
            (let%map_open.Command (_ : int) = anon ("T" %: int) in
             fun () -> print_endline "executing one") )
      ; ( "two"
        , Command.basic
            ~summary:"demo"
            (let%map_open.Command (_ : string) = flag "f" (required string) ~doc:"demo" in
             fun () -> print_endline "executing two") )
      ]
  ;;

  let%expect_test "Parsing correctly one" =
    run
      ~add_validate_parsing_flag:true
      ~argv:[ "prog"; "one"; "-validate-parsing"; "1" ]
      command;
    [%expect {| (command.ml.Exit_called (status 0)) |}]
  ;;

  let%expect_test "Parsing incorrectly one" =
    run
      ~add_validate_parsing_flag:true
      ~argv:[ "prog"; "one"; "first"; "-validate-parsing" ]
      command;
    [%expect
      {|
      Error parsing command line:

        failed to parse T value "first"
        (Failure "Int.of_string: \"first\"")

      For usage information, run

        prog one -help

      (raised (command.ml.Exit_called (status 1)))
      |}]
  ;;

  let%expect_test "One help test" =
    run ~add_validate_parsing_flag:true ~argv:[ "prog"; "one"; "-help" ] command;
    [%expect
      {|
      demo

        prog one T

      === flags ===

        [-validate-parsing]        . validate arguments are parsed correctly and exit
                                     immediately
        [-help], -?                . print this help text and exit

      (command.ml.Exit_called (status 0))
      |}]
  ;;

  let%expect_test "One normal execution" =
    run ~add_validate_parsing_flag:true ~argv:[ "prog"; "one"; "1" ] command;
    [%expect {| executing one |}]
  ;;

  let%expect_test "Only available in subcommands" =
    run ~add_validate_parsing_flag:true ~argv:[ "prog"; "-help" ] command;
    [%expect
      {|
      This implements a basic command made of two different groups

        prog SUBCOMMAND

      === subcommands ===

        one                        . demo
        two                        . demo
        version                    . print version information
        help                       . explain a given subcommand (perhaps recursively)

      (command.ml.Exit_called (status 0))
      |}]
  ;;

  let%expect_test "Parsing correctly two" =
    run
      ~add_validate_parsing_flag:true
      ~argv:[ "prog"; "two"; "-validate-parsing"; "-f"; "foo" ]
      command;
    [%expect {| (command.ml.Exit_called (status 0)) |}]
  ;;
end
