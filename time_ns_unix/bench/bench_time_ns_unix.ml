open! Core
include Core.Time_ns
open Time_ns_unix

module Constants = struct
  let opaque = Sys.opaque_identity
  let date = opaque (Date.create_exn ~y:2013 ~m:Oct ~d:07)
  let example_ofday = Ofday.create ~hr:13 ~min:29 ~sec:59 ~ms:654 ~us:321 () |> opaque
  let example = of_date_ofday ~zone:Zone.utc date example_ofday |> opaque
  let example_formatted = format example "%F %T%z" ~zone:Zone.utc |> opaque
end

open Constants

let pause = pause
let pause_forever = pause_forever
let interruptible_pause = interruptible_pause
let format = format
let%bench "format" = format example "%F %T%z" ~zone:Zone.utc

let%bench "format locale" =
  format ~locale:(force Core_unix.Locale.posix) example "%F %T%z" ~zone:Zone.utc
;;

let parse = parse
let%bench "parse" = parse example_formatted ~fmt:"%F %T%z" ~zone:Zone.utc

let%bench "parse locale" =
  parse
    ~locale:(force Core_unix.Locale.posix)
    example_formatted
    ~fmt:"%F %T%z"
    ~zone:Zone.utc
;;
