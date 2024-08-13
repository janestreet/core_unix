open! Core
module Unix = Core_unix
module Time = Time_float_unix

(* The signature ensures we match the signature of [Time_float_unix]. Things in
   [Time_float] are already covered by core's benchmarks so we skip them here. *)
include Time_float
open Time

module Time_constants = struct
  let opaque = Sys.opaque_identity
  let date = opaque (Date.create_exn ~y:2013 ~m:Oct ~d:07)
  let example_ofday = Ofday.create ~hr:13 ~min:29 ~sec:59 ~ms:654 ~us:321 () |> opaque
  let example = of_date_ofday ~zone:Zone.utc date example_ofday |> opaque
  let example_formatted = format example "%F %T%z" ~zone:Zone.utc |> opaque
  let epoch_tm = Unix.gmtime 0. |> opaque
end

open Time_constants

(* don't benchmark pauses *)
let pause = pause
let interruptible_pause = interruptible_pause
let pause_forever = pause_forever
let of_tm = of_tm
let%bench "of_tm" = of_tm epoch_tm ~zone:Zone.utc
let format = format
let%bench "format" = format example "%F %T%z" ~zone:Zone.utc
let parse = parse
let%bench "parse" = parse example_formatted ~fmt:"%F %T%z" ~zone:Zone.utc
