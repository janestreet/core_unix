open! Core
open! Int.Replace_polymorphic_compare

module Stable = struct
  open Stable_witness.Export

  module V1 = struct
    module T = struct
      type 'a t =
        | Interval of 'a * 'a
        | Empty
      [@@deriving
        bin_io ~localize
        , of_sexp
        , variants
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp_grammar
        , stable_witness]

      type 'a interval = 'a t
      [@@deriving
        bin_io ~localize
        , of_sexp
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp_grammar
        , stable_witness]

      let interval_of_sexp a_of_sexp sexp =
        try interval_of_sexp a_of_sexp sexp (* for backwards compatibility *) with
        | _exn ->
          (match sexp with
           | Sexp.List [] -> Empty
           | Sexp.List [ lb; ub ] -> Interval (a_of_sexp lb, a_of_sexp ub)
           | Sexp.Atom _ | Sexp.List _ ->
             of_sexp_error "Interval.t_of_sexp: expected pair or empty list" sexp)
      ;;

      let sexp_of_interval sexp_of_a t =
        match t with
        | Empty -> Sexp.List []
        | Interval (lb, ub) -> Sexp.List [ sexp_of_a lb; sexp_of_a ub ]
      ;;

      let interval_sexp_grammar a_sexp_grammar =
        Sexplib0.Sexp_grammar.coerce
          { untyped =
              Union
                [ (interval_sexp_grammar a_sexp_grammar).untyped
                ; List Empty
                ; List
                    (Cons (a_sexp_grammar.untyped, Cons (a_sexp_grammar.untyped, Empty)))
                ]
          }
      ;;
    end

    open T

    type 'a t = 'a interval
    [@@deriving
      sexp
      , bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , hash
      , sexp_grammar
      , stable_witness]

    module Float = struct
      module T = struct
        type t = float interval
        [@@deriving
          sexp
          , bin_io ~localize
          , compare ~localize
          , equal ~localize
          , hash
          , sexp_grammar
          , stable_witness]
      end

      include T
      include Comparator.Stable.V1.Make (T)
    end

    module Int = struct
      module T = struct
        type t = int interval
        [@@deriving
          sexp
          , bin_io ~localize
          , compare ~localize
          , equal ~localize
          , hash
          , sexp_grammar
          , stable_witness]
      end

      include T
      include Comparator.Stable.V1.Make (T)
    end

    module Time = struct end
    module Time_ns = struct end

    module Ofday = struct
      module T = struct
        type t = Core.Time_float.Stable.Ofday.V1.t interval
        [@@deriving
          sexp
          , bin_io ~localize
          , compare ~localize
          , equal ~localize
          , hash
          , sexp_grammar
          , stable_witness]
      end

      include T
      include Comparator.Stable.V1.Make (T)
    end

    module Ofday_ns = struct
      module T = struct
        type t = Core.Time_ns.Stable.Ofday.V1.t interval
        [@@deriving
          sexp
          , bin_io ~localize
          , compare ~localize
          , equal ~localize
          , hash
          , sexp_grammar
          , stable_witness]
      end

      include T
      include Comparator.Stable.V1.Make (T)
    end

    module Private = struct
      include T

      let to_float t = t
      let to_int t = t
      let to_ofday t = t
      let to_time t = t
    end
  end
end

open Stable.V1.T

module type Bound = sig
  type 'a bound

  val compare : 'a bound -> 'a bound -> int
  val ( >= ) : 'a bound -> 'a bound -> bool
  val ( <= ) : 'a bound -> 'a bound -> bool
  val ( = ) : 'a bound -> 'a bound -> bool
  val ( > ) : 'a bound -> 'a bound -> bool
  val ( < ) : 'a bound -> 'a bound -> bool
  val ( <> ) : 'a bound -> 'a bound -> bool
end

module Raw_make (T : Bound) = struct
  module T = struct
    include T

    let _ = ( <> ) (* Prevent unused value warning for "<>" *)
    let max x y = if T.( >= ) x y then x else y
    let min x y = if T.( <= ) x y then x else y
  end

  module Interval = struct
    let empty = Empty

    let is_malformed = function
      | Empty -> false
      | Interval (x, y) -> T.( > ) x y
    ;;

    let empty_cvt = function
      | Empty -> Empty
      | Interval (x, y) as i -> if T.( > ) x y then Empty else i
    ;;

    let create x y =
      (* if x > y, then this is just the Empty interval. *)
      empty_cvt (Interval (x, y))
    ;;

    let intersect i1 i2 =
      match i1, i2 with
      | Empty, _ | _, Empty -> Empty
      | Interval (l1, u1), Interval (l2, u2) ->
        empty_cvt (Interval (T.max l1 l2, T.min u1 u2))
    ;;

    let is_empty = function
      | Empty -> true
      | _ -> false
    ;;

    let is_empty_or_singleton = function
      | Empty -> true
      | Interval (x, y) -> T.( = ) x y
    ;;

    let bounds = function
      | Empty -> None
      | Interval (l, u) -> Some (l, u)
    ;;

    let lbound = function
      | Empty -> None
      | Interval (l, _) -> Some l
    ;;

    let ubound = function
      | Empty -> None
      | Interval (_, u) -> Some u
    ;;

    let bounds_exn = function
      | Empty -> invalid_arg "Interval.bounds_exn: empty interval"
      | Interval (l, u) -> l, u
    ;;

    let%template[@mode m = (global, local)] lbound_exn = function
      | Empty -> invalid_arg "Interval.lbound_exn: empty interval"
      | Interval (l, _) -> l
    ;;

    let ubound_exn = function
      | Empty -> invalid_arg "Interval.ubound_exn: empty interval"
      | Interval (_, u) -> u
    ;;

    let compare_value i x =
      match i with
      | Empty -> `Interval_is_empty
      | Interval (l, u) ->
        if T.( < ) x l then `Below else if T.( > ) x u then `Above else `Within
    ;;

    let contains i x = Poly.( = ) (compare_value i x) `Within

    let bound i x =
      match i with
      | Empty -> None
      | Interval (l, u) ->
        let bounded_value = if T.( < ) x l then l else if T.( < ) u x then u else x in
        Some bounded_value
    ;;

    let is_superset i1 ~of_:i2 =
      match i1, i2 with
      | Interval (l1, u1), Interval (l2, u2) -> T.( <= ) l1 l2 && T.( >= ) u1 u2
      | _, Empty -> true
      | Empty, Interval (_, _) -> false
    ;;

    let is_subset i1 ~of_:i2 = is_superset i2 ~of_:i1

    let map t ~f =
      match t with
      | Empty -> Empty
      | Interval (l, u) -> empty_cvt (Interval (f l, f u))
    ;;

    let interval_compare t1 t2 =
      match t1, t2 with
      | Empty, Empty -> 0
      | Empty, Interval _ -> -1
      | Interval _, Empty -> 1
      | Interval (l1, u1), Interval (l2, u2) ->
        let c = T.compare l1 l2 in
        if Int.( <> ) c 0 then c else T.compare u1 u2
    ;;

    let are_disjoint_gen ~are_disjoint intervals =
      let intervals = Array.of_list intervals in
      try
        for i = 0 to Array.length intervals - 1 do
          for j = i + 1 to Array.length intervals - 1 do
            if not (are_disjoint intervals.(i) intervals.(j)) then raise Exit
          done
        done;
        true
      with
      | Exit -> false
    ;;

    let are_disjoint intervals =
      are_disjoint_gen intervals ~are_disjoint:(fun i1 i2 -> is_empty (intersect i1 i2))
    ;;

    let are_disjoint_as_open_intervals intervals =
      are_disjoint_gen intervals ~are_disjoint:(fun i1 i2 ->
        is_empty_or_singleton (intersect i1 i2))
    ;;

    let list_intersect ilist1 ilist2 =
      if (not (are_disjoint ilist1)) || not (are_disjoint ilist2)
      then invalid_arg "Interval.list_intersect: non-disjoint input list";
      let pairs = List.cartesian_product ilist1 ilist2 in
      List.filter_map pairs ~f:(fun (i1, i2) ->
        let i = intersect i1 i2 in
        if is_empty i then None else Some i)
    ;;

    let half_open_intervals_are_a_partition intervals =
      let intervals = List.filter ~f:(fun x -> not (is_empty x)) intervals in
      let intervals = List.sort ~compare:interval_compare intervals in
      (* requires sorted list of intervals *)
      let rec is_partition a = function
        | [] -> true
        | b :: tl -> T.( = ) (ubound_exn a) (lbound_exn b) && is_partition b tl
      in
      match intervals with
      | [] -> true
      | x :: xs -> is_partition x xs
    ;;

    let convex_hull intervals =
      List.fold intervals ~init:empty ~f:(fun i1 i2 ->
        (* Compute the convex hull of two intervals *)
        match bounds i1, bounds i2 with
        | None, _ -> i2
        | _, None -> i1
        | Some (l1, u1), Some (l2, u2) -> create (T.min l1 l2) (T.max u1 u2))
    ;;
  end

  module Set = struct
    (* The intervals are sorted by their lower bound *)
    let drop_empty_intervals_and_sort intervals =
      List.filter intervals ~f:(fun i -> not (Interval.is_empty i))
      |> List.sort ~compare:(Comparable.lift T.compare ~f:Interval.lbound_exn)
    ;;

    let create_from_intervals_exn intervals =
      let intervals = drop_empty_intervals_and_sort intervals in
      if not (Interval.are_disjoint intervals)
      then failwith "Interval_set.create: intervals were not disjoint"
      else intervals
    ;;

    let create_merging_intervals intervals =
      (* We only need to check for overlapping intervals that are adjacent in the sorted
         order.  That's because, if you have intervals [abc] that are sorted by their
         lower-bound, if a intersects with c, then b must intersect with c as well.

         As a result we can just iteratively merge together adjacent intervals that
         intersect, and that will capture all necessary merges.  *)
      drop_empty_intervals_and_sort intervals
      |> List.fold ~init:[] ~f:(fun acc interval ->
        match acc with
        | [] -> [ interval ]
        | prev_interval :: tl ->
          if Interval.are_disjoint [ prev_interval; interval ]
          then interval :: acc
          else Interval.convex_hull [ prev_interval; interval ] :: tl)
      |> List.rev
    ;;

    let create_exn pair_list =
      let intervals =
        List.map pair_list ~f:(fun (lbound, ubound) -> Interval.create lbound ubound)
      in
      create_from_intervals_exn intervals
    ;;

    let contains_set ~container ~contained =
      List.for_all contained ~f:(fun contained_interval ->
        List.exists container ~f:(fun container_interval ->
          Interval.is_superset container_interval ~of_:contained_interval))
    ;;

    let contains t x = List.exists t ~f:(fun interval -> Interval.contains interval x)

    let ubound_exn t =
      match t with
      | [] -> invalid_arg "Interval_set.ubound called on empty set"
      | _ -> Interval.ubound_exn (List.last_exn t)
    ;;

    let lbound_exn t =
      match t with
      | [] -> invalid_arg "Interval_set.lbound called on empty set"
      | _ -> Interval.lbound_exn (List.hd_exn t)
    ;;

    let ubound t =
      match List.last t with
      | None -> None
      | Some i ->
        (match Interval.ubound i with
         | None -> assert false
         | Some x -> Some x)
    ;;

    let lbound t =
      match List.hd t with
      | None -> None
      | Some i ->
        (match Interval.lbound i with
         | None -> assert false
         | Some x -> Some x)
    ;;

    let union_list ts = List.concat_no_order ts |> create_merging_intervals
    let union t1 t2 = union_list [ t1; t2 ]
    let inter t1 t2 = Interval.list_intersect t1 t2 |> create_from_intervals_exn
  end
end

type 'a t = 'a interval
[@@deriving bin_io ~localize, sexp, compare ~localize, equal ~localize, hash]

module C = Raw_make (struct
    type 'a bound = 'a

    include Poly
  end)

include C.Interval

let t_of_sexp a_of_sexp s =
  let t = t_of_sexp a_of_sexp s in
  if is_malformed t then of_sexp_error "Interval.t_of_sexp error: malformed input" s;
  t
;;

module Set = struct
  type 'a t = 'a interval list
  [@@deriving bin_io ~localize, sexp, compare ~localize, equal ~localize, hash]

  include C.Set
end

[%%template
[@@@mode.default m = (global, local)]

module Make (Bound : sig
    type t
    [@@deriving (bin_io [@mode m]), (compare [@mode m]), (equal [@mode m]), hash, sexp]

    include Comparable.S [@mode m] with type t := t
  end) =
struct
  type t = Bound.t interval
  [@@deriving (bin_io [@mode m]), sexp, (compare [@mode m]), (equal [@mode m]), hash]

  type interval = t [@@deriving (bin_io [@mode m]), sexp]
  type bound = Bound.t

  module C = Raw_make (struct
      type 'a bound = Bound.t

      let compare = Bound.compare

      include (Bound : Comparable.Infix with type t := Bound.t)
    end)

  include C.Interval

  let to_poly (t : t) = t

  let t_of_sexp s =
    let t = t_of_sexp s in
    if is_malformed t
    then
      failwithf "Interval.Make.t_of_sexp error: malformed input %s" (Sexp.to_string s) ()
    else t
  ;;

  module Set = struct
    type t = interval list [@@deriving sexp, (bin_io [@mode m])]

    include C.Set

    let to_poly (t : t) = t
    let to_list (t : t) : interval list = t
  end
end

module type S1 = Interval_intf.S1 [@mode m]

module type S =
  Interval_intf.S [@mode m] with type 'a poly_t := 'a t with type 'a poly_set := 'a Set.t]

module type S_time = sig end

module%template Float = Make [@mode local] (Float)
module%template Ofday = Make [@mode local] (Core.Time_float.Ofday)
module%template Ofday_ns = Make [@mode local] (Core.Time_ns.Ofday)

module Int = struct
  include%template Make [@mode local] (Int)

  let length t =
    match t with
    | Empty -> 0
    | Interval (lo, hi) ->
      let len = 1 + hi - lo in
      (* If [hi] and [lo] are far enough apart (e.g. if [lo <= 0] and
         [hi = Int.max_value]), [len] will overlow. *)
      if len < 0
      then
        failwiths
          "interval length not representable"
          ([%globalize: int interval] t)
          [%sexp_of: t];
      len
  ;;

  let get t i =
    let fail () =
      failwiths
        "index out of bounds"
        (i, [%globalize: int interval] t)
        [%sexp_of: int * t]
    in
    match t with
    | Empty -> fail () [@nontail]
    | Interval (lo, hi) ->
      if i < 0 then fail ();
      let x = lo + i in
      if x < lo || x > hi then fail ();
      x
  ;;

  let iter t ~f =
    match t with
    | Empty -> ()
    | Interval (lo, hi) ->
      for x = lo to hi do
        f x
      done
  ;;

  let fold =
    let rec fold_interval ~lo ~hi ~acc ~f =
      if lo = hi then f acc hi else fold_interval ~lo:(lo + 1) ~hi ~acc:(f acc lo) ~f
    in
    fun t ~init ~f ->
      match t with
      | Empty -> init
      | Interval (lo, hi) -> fold_interval ~lo ~hi ~acc:init ~f
  ;;

  module For_container = Container.Make0 (struct
      type nonrec t = t

      module Elt = Int

      let iter = `Custom iter
      let fold = fold
      let length = `Custom length
    end)

  let exists = For_container.exists
  let for_all = For_container.for_all
  let sum = For_container.sum
  let count = For_container.count
  let find = For_container.find
  let find_map = For_container.find_map
  let to_list = For_container.to_list
  let to_array = For_container.to_array
  let fold_result = For_container.fold_result
  let fold_until = For_container.fold_until

  let min_elt t ~(local_ compare : _ -> _ -> _) =
    if not (phys_equal compare Int.compare)
    then For_container.min_elt t ~compare
    else lbound t
  ;;

  let max_elt t ~(local_ compare : _ -> _ -> _) =
    if not (phys_equal compare Int.compare)
    then For_container.max_elt t ~compare
    else ubound t
  ;;

  let mem t x = contains t x

  (* Note that we use zero-based indexing here, because that's what Binary_searchable
     requires, even though at the end we want to export functions that use the natural
     bounds of the interval.  *)
  module%template For_binary_search = Binary_searchable.Make [@mode local] (struct
      type nonrec t = t
      type nonrec elt = bound

      [%%template
      [@@@mode.default m = (local, global)]

      let length = length
      let[@inline] get t i = get t i]
    end)

  [%%template
  [@@@mode.default m = (global, local)]

  let binary_search ?pos ?len t ~compare which elt = exclave_
    let zero_based_pos = Option.map pos ~f:(fun x -> x - (lbound_exn [@mode m]) t) in
    let zero_based_result =
      (For_binary_search.binary_search [@mode m])
        ?pos:zero_based_pos
        ?len
        t
        ~compare
        which
        elt
    in
    (Option.map [@mode local]) zero_based_result ~f:(fun x ->
      x + (lbound_exn [@mode m]) t)
  ;;

  let binary_search_segmented ?pos ?len t ~segment_of which = exclave_
    let zero_based_pos = Option.map pos ~f:(fun x -> x - (lbound_exn [@mode m]) t) in
    let zero_based_result =
      (For_binary_search.binary_search_segmented [@mode m])
        ?pos:zero_based_pos
        ?len
        t
        ~segment_of
        which
    in
    (Option.map [@mode local]) zero_based_result ~f:(fun x ->
      x + (lbound_exn [@mode m]) t)
  ;;]

  module Private = struct
    let[@inline] get t i = get t i
  end
end

module Private = struct
  module%template [@mode m = (global, local)] Make = Make [@mode m]
end

module Time = struct end
module Time_ns = struct end
