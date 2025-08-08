open! Core
module Unix = Core_unix

let random = lazy (Random.State.make_self_init ())

let read_file_and_convert ~of_string path =
  Option.try_with (fun () -> In_channel.read_all path |> String.strip |> of_string)
;;

let default_max_retry_delay ~timeout =
  let default_delay = Time_float.Span.of_int_ms 300 in
  match timeout with
  | None -> default_delay
  | Some timeout -> Time_float.Span.min default_delay (Time_float.Span.( / ) timeout 3.)
;;

let wait_at_most max_delay random =
  let delay = Random.State.float (Lazy.force random) (Time_float.Span.to_sec max_delay) in
  ignore (Unix.nanosleep delay : float)
;;

(* no timeout specified = wait indefinitely *)
let repeat_with_timeout ?max_retry_delay ?(random = random) ?timeout lockf path =
  let max_retry_delay =
    match max_retry_delay with
    | Some delay -> delay
    | None -> default_max_retry_delay ~timeout
  in
  match timeout with
  | None ->
    let rec loop () =
      match lockf path with
      | Ok res -> res
      | Error (`Retriable _e) ->
        wait_at_most max_retry_delay random;
        loop ()
    in
    loop ()
  | Some timeout ->
    let start_time = Time_float.now () in
    let rec loop () =
      match lockf path with
      | Ok res -> res
      | Error (`Retriable e) ->
        let since_start = Time_float.abs_diff start_time (Time_float.now ()) in
        if Time_float.Span.(since_start > timeout)
        then
          failwithf
            "Lock_file: '%s' timed out waiting for existing lock. Last error was %s"
            path
            (Exn.to_string e)
            ()
        else (
          wait_at_most max_retry_delay random;
          loop ())
    in
    loop ()
;;
