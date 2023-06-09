Test validate parsing succeeds
  $ $TEST_DIR/../test-bin/command_validate_parsing.exe 4 -f foo -validate-parsing

Test validate parsing fails
  $ $TEST_DIR/../test-bin/command_validate_parsing.exe foo -f foo -validate-parsing
  Error parsing command line:
  
    failed to parse T value "foo"
    (Failure "Int.of_string: \"foo\"")
  
  For usage information, run
  
    command_validate_parsing.exe -help
  
  [1]

Test without validate parsing
  $ $TEST_DIR/../test-bin/command_validate_parsing.exe 4 -f 8 
  executing

