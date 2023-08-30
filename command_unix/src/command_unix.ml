open! Core
module Path = Command.Private.Path

module For_unix = Command.Private.For_unix (struct
  module Pid = Pid
  module Signal = Signal
  module Thread = Core_thread

  module Unix = struct
    include Core_unix

    let unsafe_getenv = Sys_unix.unsafe_getenv
    let create_process_env = create_process_env ?setpgid:None
    let wait pid = ignore (wait (`Pid pid) : Pid.t * Exit_or_signal.t)
  end

  module Version_util = struct
    include Version_util
    module Time = Time_float_unix
  end
end)

let run = For_unix.run
let shape = For_unix.shape

module Deprecated = struct
  let run = For_unix.deprecated_run
end

module Shape = struct
  let help_text = For_unix.help_for_shape
end
