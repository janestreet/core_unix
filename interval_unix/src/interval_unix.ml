open! Core
module Interval = Interval_lib.Interval

module Stable = struct
  module V1 = struct
    module Time = struct
      module T = struct
        type t = Time_float_unix.Stable.V1.t Interval.Stable.V1.t
        [@@deriving sexp, bin_io, compare]
      end

      include T
      include Comparator.Stable.V1.Make (T)
    end

    module Time_ns = struct
      module T = struct
        type t = Time_ns_unix.Stable.V1.t Interval.Stable.V1.t
        [@@deriving sexp, bin_io, compare]
      end

      include T
      include Comparator.Stable.V1.Make (T)
    end
  end
end

module type S_time = Interval_unix_intf.S_time

module type Time_bound = sig
  type t [@@deriving bin_io, sexp, compare, hash]

  include Comparable.S with type t := t

  module Ofday : sig
    type t
  end

  module Zone : sig
    type t

    val local : t Lazy.t
  end

  val occurrence
    :  [ `First_after_or_at | `Last_before_or_at ]
    -> t
    -> ofday:Ofday.t
    -> zone:Zone.t
    -> t
end

module Make_time (Time : Time_bound) = struct
  include Interval.Private.Make (Time)

  let create_ending_after ?zone (open_ofday, close_ofday) ~now =
    let zone =
      match zone with
      | None -> Lazy.force Time.Zone.local
      | Some z -> z
    in
    let close_time = Time.occurrence `First_after_or_at now ~zone ~ofday:close_ofday in
    let open_time =
      Time.occurrence `Last_before_or_at close_time ~zone ~ofday:open_ofday
    in
    create open_time close_time
  ;;

  let create_ending_before ?zone (open_ofday, close_ofday) ~ubound =
    let zone =
      match zone with
      | None -> Lazy.force Time.Zone.local
      | Some z -> z
    in
    let close_time = Time.occurrence `Last_before_or_at ubound ~zone ~ofday:close_ofday in
    let open_time =
      Time.occurrence `Last_before_or_at close_time ~zone ~ofday:open_ofday
    in
    create open_time close_time
  ;;
end

module Time = Make_time (Time_float_unix)
module Time_ns = Make_time (Time_ns_unix)
