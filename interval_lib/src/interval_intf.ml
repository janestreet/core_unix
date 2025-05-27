open! Core

module type Gen = sig
  type 'a t

  (** [bound] is the type of points in the interval (and therefore of the bounds). [bound]
      is instantiated in two different ways below: in [module type S] as a monotype and in
      [module type S1] as ['a]. *)
  type 'a bound

  (** [create l u] returns the interval with lower bound [l] and upper bound [u], unless
      [l > u], in which case it returns the empty interval. *)
  val create : 'a bound -> 'a bound -> 'a t

  val empty : 'a t
  val intersect : 'a t -> 'a t -> 'a t
  val is_empty : 'a t -> bool
  val is_empty_or_singleton : 'a t -> bool

  (*_ If you are looking for a simple interval type where the bounds are not optional,
    consider Min_max_pair.t. *)
  val bounds : 'a t -> ('a bound * 'a bound) option
  val lbound : 'a t -> 'a bound option
  val ubound : 'a t -> 'a bound option
  val bounds_exn : 'a t -> 'a bound * 'a bound
  val lbound_exn : 'a t -> 'a bound
  val ubound_exn : 'a t -> 'a bound

  (** [convex_hull ts] returns an interval whose upper bound is the greatest upper bound
      of the intervals in the list, and whose lower bound is the least lower bound of the
      list.

      Suppose you had three intervals [a], [b], and [c]:

      {v
             a:  (   )
             b:    (     )
             c:            ( )

          hull:  (           )
      v}

      In this case the hull goes from [lbound_exn a] to [ubound_exn c]. *)
  val convex_hull : 'a t list -> 'a t

  val contains : 'a t -> 'a bound -> bool

  val compare_value
    :  'a t
    -> 'a bound
    -> [ `Below | `Within | `Above | `Interval_is_empty ]

  (** [bound t x] returns [None] iff [is_empty t]. If [bounds t = Some (a, b)], then
      [bound] returns [Some y] where [y] is the element of [t] closest to [x]. I.e.:

      {v
        y = a  if x < a
        y = x  if a <= x <= b
        y = b  if x > b
      v} *)
  val bound : 'a t -> 'a bound -> 'a bound option

  (** [is_superset i1 of_:i2] is whether i1 contains i2. The empty interval is contained
      in every interval. *)
  val is_superset : 'a t -> of_:'a t -> bool

  val is_subset : 'a t -> of_:'a t -> bool

  (** [map t ~f] returns [create (f l) (f u)] if [bounds t = Some (l, u)], and [empty] if
      [t] is empty. Note that if [f l > f u], the result of [map] is [empty], by the
      definition of [create].

      If you think of an interval as a set of points, rather than a pair of its bounds,
      then [map] is not the same as the usual mathematical notion of mapping [f] over that
      set. For example, [map ~f:(fun x -> x * x)] maps the interval [[-1,1]] to [[1,1]],
      not to [[0,1]]. *)
  val map : 'a t -> f:('a bound -> 'b bound) -> 'b t

  (** [are_disjoint ts] returns [true] iff the intervals in [ts] are pairwise disjoint. *)
  val are_disjoint : 'a t list -> bool

  (** Returns true iff a given set of intervals would be disjoint if considered as open
      intervals, e.g., [(3,4)] and [(4,5)] would count as disjoint according to this
      function. *)
  val are_disjoint_as_open_intervals : 'a t list -> bool

  (** Assuming that [ilist1] and [ilist2] are lists of disjoint intervals,
      [list_intersect ilist1 ilist2] considers the intersection [(intersect i1 i2)] of
      every pair of intervals [(i1, i2)], with [i1] drawn from [ilist1] and [i2] from
      [ilist2], returning just the non-empty intersections. By construction these
      intervals will be disjoint, too. For example:

      {[
        let i = Interval.create;;

        list_intersect [ i 4 7; i 9 15 ] [ i 2 4; i 5 10; i 14 20 ];;
        [ (4, 4), (5, 7), (9, 10), (14, 15) ]
      ]}

      Raises an exception if either input list is non-disjoint. *)
  val list_intersect : 'a t list -> 'a t list -> 'a t list

  (** Returns true if the intervals, when considered as half-open intervals, nestle up
      cleanly one to the next. I.e., if you sort the intervals by the lower bound, then
      the upper bound of the [n]th interval is equal to the lower bound of the [n+1]th
      interval. The intervals do not need to partition the entire space, they just need to
      partition their union. *)
  val half_open_intervals_are_a_partition : 'a t list -> bool
end

module type Gen_set = sig
  type 'a t
  type 'a bound

  (** An interval set is a set of nonempty disjoint intervals. *)
  type 'a interval

  (** [create_exn] creates an interval set containing intervals whose lower and upper
      bounds are given by the pairs passed to the function. Raises if the pairs overlap. *)
  val create_exn : ('a bound * 'a bound) list -> 'a t

  (** [create_from_intervals_exn] creates an interval set. Empty intervals are dropped.
      Raises if the nonempty intervals are not disjoint. *)
  val create_from_intervals_exn : 'a interval list -> 'a t

  val contains : 'a t -> 'a bound -> bool

  (** [contains_set] returns true iff for every interval in the contained set, there
      exists an interval in the container set that is its superset. *)
  val contains_set : container:'a t -> contained:'a t -> bool

  (** The largest and smallest element of the interval set, respectively. Raises
      Invalid_argument on empty sets. *)
  val ubound_exn : 'a t -> 'a bound

  val lbound_exn : 'a t -> 'a bound
  val ubound : 'a t -> 'a bound option
  val lbound : 'a t -> 'a bound option

  (** [inter t1 t2] computes the intersection of sets [t1] and [t2].
      [O(length t1 * length t2)]. *)
  val inter : 'a t -> 'a t -> 'a t

  (** [union t1 t2] computes the union of sets [t1] and [t2].
      [O((length t1 + length t2) * log(length t1 + length t2))]. *)
  val union : 'a t -> 'a t -> 'a t

  (** [union_list l] computes the union of a list of sets.
      [O(sum length * log(sum length))]. *)
  val union_list : 'a t list -> 'a t
end

[%%template
[@@@mode.default m = (global, local)]

module type S = sig
  type t
  [@@deriving (bin_io [@mode m]), sexp, (compare [@mode m]), (equal [@mode m]), hash]

  type bound

  include Gen with type 'a t := t with type 'a bound := bound (** @inline *)

  (** [create] has the same type as in [Gen], but adding it here prevents a type-checker
      issue with nongeneralizable type variables. *)
  val create : bound -> bound -> t

  type 'a poly_t

  val to_poly : t -> bound poly_t

  type 'a poly_set

  module Set : sig
      type t [@@deriving bin_io, sexp]

      include Gen_set with type 'a t := t with type 'a bound := bound (** @inline *)

      val to_poly : t -> bound poly_set

      (** [to_list] will return a list of non-overlapping intervals defining the set, in
          ascending order. *)
      val to_list : t -> bound interval list
    end
    with type 'a interval := t
end

module type S1 = sig
  (** This type [t] supports bin-io and sexp conversion by way of the
      [[@@deriving bin_io, sexp]] extensions, which inline the relevant function
      signatures (like [bin_read_t] and [t_of_sexp]). *)
  type 'a t
  [@@deriving (bin_io [@mode m]), sexp, (compare [@mode m]), (equal [@mode m]), hash]

  include Gen with type 'a t := 'a t with type 'a bound := 'a (** @inline *)

  module Set : sig
      type 'a t [@@deriving bin_io, sexp]

      include Gen_set with type 'a t := 'a t with type 'a bound := 'a (** @inline *)
    end
    with type 'a interval := 'a t
end

module type S_stable = sig
  type t [@@deriving (equal [@mode m]), hash, sexp_grammar]

  include Stable_with_witness [@mode m] with type t := t
end]

(** Module for simple closed intervals over arbitrary types. Used by calling the
    {{!module:Core.Interval.Make} [Make]} functor with a type that satisfies
    {{!module:Base.Comparable} [Comparable]} (for correctly ordering elements).

    Note that the actual interface for intervals is in
    {{!modtype:Core__.Interval_intf.Gen} [Interval_intf.Gen]}, following a Core pattern of
    defining an interface once in a [Gen] module, then reusing it across monomorphic ([S])
    and polymorphic ([S1], [S2], ... [SN]) variants, where [SN] denotes a signature of N
    parameters. Here, [S1] is included in this module because the signature of one ['a]
    parameter is the default.

    See the documentation of {{!module:Core.Interval.Make} [Interval.Make]} for a more
    detailed usage example. *)
module type%template Interval = sig
  (** {2 Intervals using polymorphic compare}

      This part of the interface is for polymorphic intervals, which are well ordered by
      polymorphic compare. Using this with types that are not (like sets) will lead to
      crazy results. *)
  include S1 [@mode local]
  (** @inline *)

  (** {2 Type-specialized intervals}

      The module type [S] is used to define signatures for intervals over a specific type,
      like [Interval.Ofday] (whose bounds are [Time.Ofday.t]) or [Interval.Float], whose
      bounds are floats.

      Note the heavy use of destructive substitution, which removes the redefined type or
      module from the signature. This allows for clean type constraints in codebases, like
      Core's, where there are lots of types going by the same name (e.g., "t"). *)

  (** {3 Signatures}

      The following signatures are used for specifying the types of the type-specialized
      intervals. *)

  module type [@mode m = (global, local)] S1 = S1 [@mode m]

  module type [@mode m = (global, local)] S =
    S [@mode m] with type 'a poly_t := 'a t with type 'a poly_set := 'a Set.t

  module type S_time = sig end [@@deprecated "[since 2021-08] Use [Interval_unix]"]

  (** {3 Specialized interval types} *)

  module Ofday :
    S [@mode local] with type bound = Time_float.Ofday.t and type t = Time_float.Ofday.t t

  module Ofday_ns :
    S [@mode local] with type bound = Time_ns.Ofday.t and type t = Time_ns.Ofday.t t

  module Time : sig end [@@deprecated "[since 2021-08] Use [Interval_unix]"]
  module Time_ns : sig end [@@deprecated "[since 2021-08] Use [Interval_unix]"]
  module Float : S [@mode local] with type bound = Float.t and type t = Float.t t

  module Int : sig
    include S [@mode local] with type bound = Int.t (** @open *)

    include Container.S0 with type t := t with type elt := bound
    include Binary_searchable.S [@mode local] with type t := t with type elt := bound

    (**/**)

    (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

      https://opensource.janestreet.com/standards/#private-submodules *)
    module Private : sig
      val get : t -> int -> int
    end
  end

  (** [Interval.Make] is a functor that takes a type that you'd like to create intervals
      for and returns a module with functions over intervals of that type.

      For example, suppose you had a [Percent.t] type and wanted to work with intervals
      over it, i.e., inclusive ranges like 40-50% or 0-100%. You would create your
      [Percent_interval] module by calling:

      {[
        module Percent_interval = Interval.Make (Percent)
      ]}

      You now have a module with lots of functionality ready to use. For instance you
      could call [Percent_interval.empty] to create an empty interval, or:

      {[
        Percent_interval.create (Percent.of_percentage 3) (Percent.of_percentage 30)
      ]}

      to get an actual interval that ranges from [3%] to [30%]. You can then ask questions
      of this interval, like whether it's a {{!val:is_subset} subset} of another interval
      or whether it {!val:contains} a particular value.

      NB. In order to use the [Interval.Make] functor, your type must satisfy Comparable
      and support bin-io and s-expression conversion. At a minimum, then, [Percent] must
      look like this:

      {[
        module Percent = struct
          module T = struct
            type t = float [@@deriving bin_io, compare, equal, hash, sexp]
          end

          include T
          include Comparable.Make_binable (T)
        end
      ]} *)
  module
    [@mode m = (global, local)] Make (Bound : sig
      type t
      [@@deriving (bin_io [@mode m]), (compare [@mode m]), (equal [@mode m]), hash, sexp]

      include Comparable.S [@mode m] with type t := t
    end) : S [@mode m] with type bound = Bound.t and type t = Bound.t t

  (** [Stable] is used to build stable protocols. It ensures backwards compatibility by
      checking the sexp and bin-io representations of a given module. Here it's also
      applied to the [Float], [Int], [Time], [Time_ns], and [Ofday] intervals. *)
  module Stable : sig
    module V1 : sig
      type nonrec 'a t = 'a t
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , hash
        , sexp
        , sexp_grammar
        , stable_witness]

      module Float : S_stable [@mode local] with type t = Float.t
      module Int : S_stable [@mode local] with type t = Int.t
      module Time : sig end [@@deprecated "[since 2021-08] Use [Interval_unix]"]
      module Time_ns : sig end [@@deprecated "[since 2021-08] Use [Interval_unix]"]
      module Ofday : S_stable [@mode local] with type t = Ofday.t
      module Ofday_ns : S_stable [@mode local] with type t = Ofday_ns.t

      (**/**)

      (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

        https://opensource.janestreet.com/standards/#private-submodules *)
      module Private : sig
        type 'a interval := 'a t

        type 'a t =
          | Interval of 'a * 'a
          | Empty
        [@@deriving compare ~localize, equal ~localize, hash, variants]

        val to_float : float t -> Float.t
        val to_int : int t -> Int.t
        val to_ofday : Core.Time_float.Ofday.t t -> Ofday.t

        (** Used in testing Interval_unix.Time.t. Using Core.Time.t interval is fine
            because it is equal to Interval_unix.Time.t. *)
        val to_time : Core.Time_float.t t -> Core.Time_float.t interval
      end
    end
  end

  (**/**)

  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
  module Private : sig
    module
      [@mode m = (global, local)] Make (Bound : sig
        type t [@@deriving (bin_io [@mode m]), sexp, hash]

        include Comparable.S [@mode m] with type t := t
      end) : S [@mode m] with type bound = Bound.t and type t = Bound.t t
  end
end
