open! Core
open! Import

let print_s = print_s ~sexp_style:Sexp_style.simple_pretty

let%expect_test "[create ?abbrev]" =
  let test name abbrev =
    let category = Command.Help_category.create name ?abbrev in
    print_s
      [%message
        ""
          ~name:(Command.Help_category.name category : string)
          ~abbrev:(Command.Help_category.abbrev category : string)]
  in
  test "Output options" None;
  [%expect {| ((name "Output options") (abbrev output-options)) |}];
  test " --99 Output__JSON format!? " None;
  [%expect {| ((name " --99 Output__JSON format!? ") (abbrev 99-output-json-format)) |}];
  test "Output options" (Some "out");
  [%expect {| ((name "Output options") (abbrev out)) |}];
  require_does_raise (fun () -> Command.Help_category.create " -- ");
  [%expect {| "Command.Help_category.create: [abbrev] must not be empty" |}];
  require_does_raise (fun () -> Command.Help_category.create ~abbrev:"" "Output options");
  [%expect {| "Command.Help_category.create: [abbrev] must not be empty" |}]
;;

let print_flag_categories param =
  match
    Command.Param.map param ~f:(fun _ () -> ())
    |> Command.basic ~summary:""
    |> Command_unix.shape
    |> Command.Shape.fully_forced
  with
  | Group _ | Exec _ -> assert false
  | Basic { flags; _ } ->
    List.iter flags ~f:(fun { name; help_category; _ } ->
      print_s
        [%message
          name ~_:(help_category : (Command.Shape.Help_category.t option[@sexp.option]))])
;;

let uncategorized_param =
  let%map_open.Command foo = flag "-foo" (optional string) ~doc:"_ a flag"
  and bar = flag "-bar" no_arg ~doc:" another flag" in
  foo, bar
;;

let%expect_test "flags are uncategorized by default" =
  print_flag_categories uncategorized_param;
  [%expect
    {|
    [-bar]
    "[-foo _]"
    [-help]
    |}]
;;

let%expect_test "[categorize] applies a category to all flags in a param" =
  let category = Command.Help_category.create ~abbrev:"misc" "Miscellaneous" in
  print_flag_categories (Command.Param.categorize uncategorized_param ~category);
  [%expect
    {|
    ([-bar] ((abbrev misc) (name Miscellaneous)))
    ("[-foo _]" ((abbrev misc) (name Miscellaneous)))
    [-help]
    "[-help-category CATEGORY]"
    |}]
;;

let categorized_and_uncategorized =
  let%map_open.Command foo, bar =
    categorize uncategorized_param ~category:(Command.Help_category.create "Original")
  and baz = flag "-baz" no_arg ~doc:" an uncategorized flag" in
  foo, bar, baz
;;

let%expect_test "by default, [categorize] only applies to uncategorized flags" =
  Command.Param.categorize
    categorized_and_uncategorized
    ~category:(Command.Help_category.create "Fallback")
  |> print_flag_categories;
  [%expect
    {|
    ([-bar] ((abbrev original) (name Original)))
    ([-baz] ((abbrev fallback) (name Fallback)))
    ("[-foo _]" ((abbrev original) (name Original)))
    [-help]
    "[-help-category CATEGORY]"
    |}]
;;

let%expect_test "[categorize ~override:true] replaces existing categories" =
  Command.Param.categorize
    ~override:true
    categorized_and_uncategorized
    ~category:(Command.Help_category.create "Replacement")
  |> print_flag_categories;
  [%expect
    {|
    ([-bar] ((abbrev replacement) (name Replacement)))
    ([-baz] ((abbrev replacement) (name Replacement)))
    ("[-foo _]" ((abbrev replacement) (name Replacement)))
    [-help]
    "[-help-category CATEGORY]"
    |}]
;;

let categorized_flags category first second =
  (let%map_open.Command (_ : bool) = flag first no_arg ~doc:" first"
   and (_ : bool) = flag second no_arg ~doc:" second" in
   ())
  |> Command.Param.categorize ~category
;;

let%expect_test "help for categorized flags" =
  let test param =
    let shape =
      Command.Param.map param ~f:(fun _ () -> ())
      |> Command.basic ~summary:"category test"
      |> Command_unix.shape
    in
    Command_unix.Shape.help_text
      shape
      (Command_unix.Path.create ~path_to_exe:"command")
      ~expand_dots:false
      ~flags:false
      ~recursive:false
    |> print_string
  in
  let only_uncategorized =
    let%map_open.Command () = return ()
    and (_ : bool) = flag "-zebra" no_arg ~doc:" uncategorized"
    and (_ : bool) = flag "-apple" no_arg ~doc:" uncategorized" in
    ()
  in
  let only_categorized =
    Command.Param.both
      (categorized_flags
         (Command.Help_category.create "Zeta options")
         "-zebra-zeta"
         "-apple-zeta")
      (categorized_flags
         (Command.Help_category.create ~abbrev:"alpha" "Alpha options")
         "-zebra-alpha"
         "-apple-alpha")
  in
  let with_uncategorized = Command.Param.both only_categorized only_uncategorized in
  test only_categorized;
  [%expect
    {|
    category test

      command

    === flags ===

      [-help], -?                . print this help text and exit
      [-help-category CATEGORY], -help-cat
                                 . print help for flags in this category and exit
                                   (can be: alpha, zeta-options)

    === Alpha options ===

      [-apple-alpha]             . second
      [-zebra-alpha]             . first

    === Zeta options ===

      [-apple-zeta]              . second
      [-zebra-zeta]              . first
    |}];
  test with_uncategorized;
  [%expect
    {|
    category test

      command

    === flags ===

      [-apple]                   . uncategorized
      [-zebra]                   . uncategorized
      [-help], -?                . print this help text and exit
      [-help-category CATEGORY], -help-cat
                                 . print help for flags in this category and exit
                                   (can be: alpha, zeta-options)

    === Alpha options ===

      [-apple-alpha]             . second
      [-zebra-alpha]             . first

    === Zeta options ===

      [-apple-zeta]              . second
      [-zebra-zeta]              . first
    |}]
;;

module%test [@name "-help-category"] _ = struct
  let category_test_command =
    let param =
      let%map_open.Command () =
        categorized_flags
          (Command.Help_category.create "Zeta options")
          "-zebra-zeta"
          "-apple-zeta"
      and () =
        categorized_flags
          (Command.Help_category.create ~abbrev:"alpha" "Alpha options")
          "-zebra-alpha"
          "-apple-alpha"
      and (_ : bool) = flag "-zebra" no_arg ~doc:" uncategorized"
      and (_ : bool) = flag "-apple" no_arg ~doc:" uncategorized" in
      ()
    in
    Command.basic
      ~summary:"category test"
      (let%map.Command () = param in
       fun () -> ())
  ;;

  let run_command command args =
    try Command_unix.run ~argv:("command" :: args) command with
    | exn -> print_s [%sexp (exn : exn)]
  ;;

  let run_category_test_command args = run_command category_test_command args

  let%expect_test "accepts flag and category abbreviations" =
    let test flag =
      run_category_test_command [ flag; "a" ];
      [%expect
        {|
        category test

          command

        === Alpha options ===

          [-apple-alpha]             . second
          [-zebra-alpha]             . first

        (command.ml.Exit_called (status 0))
        |}]
    in
    test "-help-category";
    test "-help-cat"
  ;;

  let%expect_test "complete category abbreviations" =
    Command_test_helpers.complete_command
      category_test_command
      ~args:[ "-help-category"; "" ];
    [%expect
      {|
      alpha
      zeta-options
      (command.ml.Exit_called (status 0))
      |}];
    Command_test_helpers.complete_command
      category_test_command
      ~args:[ "-help-category"; "alpha"; "" ];
    [%expect {| (command.ml.Exit_called (status 0)) |}]
  ;;

  let%expect_test "[validate_command] recognizes [-help-category] as built in" =
    print_s
      [%sexp
        (Command_test_helpers.validate_command
           category_test_command
           [ "-help-category"; "a" ]
         : unit Or_error.t)];
    [%expect
      {|
      category test

        CMD

      === Alpha options ===

        [-apple-alpha]             . second
        [-zebra-alpha]             . first

      (command.ml.Exit_called (status 0))
      (Ok ())
      |}]
  ;;

  let overlapping_category_command first_category second_category =
    Command.basic
      ~summary:"overlap test"
      (let%map_open.Command () = categorized_flags first_category "-first-a" "-first-b"
       and () = categorized_flags second_category "-second-a" "-second-b" in
       fun () -> ())
  ;;

  let%expect_test "categories with the same name and abbreviation are merged" =
    let command =
      overlapping_category_command
        (Command.Help_category.create "Shared options")
        (Command.Help_category.create "Shared options")
    in
    run_command command [ "-help" ];
    [%expect
      {|
      overlap test

        command

      === flags ===

        [-build-info]              . print info about this build and exit
        [-version]                 . print the version of this build and exit
        [-help], -?                . print this help text and exit
        [-help-category CATEGORY], -help-cat
                                   . print help for flags in this category and exit
                                     (can be: shared-options)

      === Shared options ===

        [-first-a]                 . first
        [-first-b]                 . second
        [-second-a]                . first
        [-second-b]                . second

      (command.ml.Exit_called (status 0))
      |}];
    run_command command [ "-help-category"; "shared-options" ];
    [%expect
      {|
      overlap test

        command

      === Shared options ===

        [-first-a]                 . first
        [-first-b]                 . second
        [-second-a]                . first
        [-second-b]                . second

      (command.ml.Exit_called (status 0))
      |}]
  ;;

  let%expect_test "abbreviations select all categories with the same name" =
    let command =
      overlapping_category_command
        (Command.Help_category.create ~abbrev:"first" "Shared options")
        (Command.Help_category.create ~abbrev:"second" "Shared options")
    in
    run_command command [ "-help" ];
    [%expect
      {|
      overlap test

        command

      === flags ===

        [-build-info]              . print info about this build and exit
        [-version]                 . print the version of this build and exit
        [-help], -?                . print this help text and exit
        [-help-category CATEGORY], -help-cat
                                   . print help for flags in this category and exit
                                     (can be: first, second)

      === Shared options ===

        [-first-a]                 . first
        [-first-b]                 . second
        [-second-a]                . first
        [-second-b]                . second

      (command.ml.Exit_called (status 0))
      |}];
    run_command command [ "-help-category"; "first" ];
    [%expect
      {|
      overlap test

        command

      === Shared options ===

        [-first-a]                 . first
        [-first-b]                 . second
        [-second-a]                . first
        [-second-b]                . second

      (command.ml.Exit_called (status 0))
      |}];
    run_command command [ "-help-category"; "second" ];
    [%expect
      {|
      overlap test

        command

      === Shared options ===

        [-first-a]                 . first
        [-first-b]                 . second
        [-second-a]                . first
        [-second-b]                . second

      (command.ml.Exit_called (status 0))
      |}];
    (* The shared name is not accepted, only the abbreviations. *)
    run_command command [ "-help-category"; "Shared options" ];
    [%expect
      {|
      Error parsing command line:

        failed to parse -help-category value "Shared options".
        (Failure "valid arguments: {first,second}")

      For usage information, run

        command -help

      (command.ml.Exit_called (status 1))
      |}]
  ;;

  let%expect_test "categories with the same abbreviation but different names raise" =
    require_does_raise (fun () ->
      overlapping_category_command
        (Command.Help_category.create ~abbrev:"shared" "First options")
        (Command.Help_category.create ~abbrev:"shared" "Second options"));
    [%expect
      {|
      ("Help categories with different names have the same abbreviation"
       (abbrev shared)
       (category_names ("First options" "Second options")))
      |}]
  ;;
end
