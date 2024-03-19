open! Core
open! Expect_test_helpers_core
module Interval = Interval_lib.Interval

let%expect_test "list_intersect" =
  let i = Interval.create in
  let x = Interval.list_intersect [ i 4 7; i 9 15 ] [ i 2 4; i 5 10; i 14 20 ] in
  print_s [%sexp (x : int Interval.t list)];
  [%expect {|
    ((4  4)
     (5  7)
     (9  10)
     (14 15))
    |}]
;;

let make_stable_unit_tests_v1 ~coerce ~non_empty =
  let module V = Base.Variant in
  let c f tests variant = f variant @ tests in
  Interval.Stable.V1.Private.Variants.fold
    ~init:[]
    ~interval:
      (c (fun interval ->
         assert (interval.V.rank = 0);
         List.map non_empty ~f:(fun ((lbound, ubound), sexp, bin_io) ->
           coerce (interval.V.constructor lbound ubound), sexp, bin_io)))
    ~empty:
      (c (fun empty ->
         assert (empty.V.rank = 1);
         [ coerce empty.V.constructor, "()", "\001" ]))
;;

let%test_module _ =
  (module struct
    open Interval.Stable

    let%test_module "Interval.V1.Float" =
      (module Stable_unit_test.Make (struct
        include V1.Float

        let equal = [%compare.equal: t]

        let tests =
          make_stable_unit_tests_v1
            ~coerce:V1.Private.to_float
            ~non_empty:
              [ ( (1.5, 120.)
                , "(1.5 120)"
                , "\000\000\000\000\000\000\000\248?\000\000\000\000\000\000^@" )
              ]
        ;;
      end))
    ;;

    let%expect_test "Interval.V1.Float.t_sexp_grammar" =
      print_endline [%sexp_digest: V1.Float.t];
      [%expect {| 6af0cb32d43233f53d0fe7d78544fe29 |}]
    ;;

    let%test_module "Interval.V1.Int" =
      (module Stable_unit_test.Make (struct
        include V1.Int

        let equal = [%compare.equal: t]

        let tests =
          make_stable_unit_tests_v1
            ~coerce:V1.Private.to_int
            ~non_empty:[ (-5, 789), "(-5 789)", "\000\255\251\254\021\003" ]
        ;;
      end))
    ;;

    let%expect_test "Interval.V1.Int.t_sexp_grammar" =
      print_endline [%sexp_digest: V1.Int.t];
      [%expect {| cc2bf35f949d1c1381da1c176ca3cce7 |}]
    ;;

    let%test_module "Interval.V1.Ofday" =
      (module Stable_unit_test.Make (struct
        include V1.Ofday

        let equal = [%compare.equal: t]

        let tests =
          let t1 = Time_float.Ofday.create ~hr:7 ~min:30 ~sec:7 ~ms:12 ~us:5 () in
          let t2 = Time_float.Ofday.create ~hr:9 ~min:45 ~sec:8 ~ms:0 ~us:1 () in
          make_stable_unit_tests_v1
            ~coerce:V1.Private.to_ofday
            ~non_empty:
              [ ( (t1, t2)
                , "(07:30:07.012005 09:45:08.000001)"
                , "\000\153\158\176\196\192_\218@\223\024\002\000\128$\225@" )
              ]
        ;;
      end))
    ;;

    let%expect_test "Interval.V1.Ofday.t_sexp_grammar" =
      print_endline [%sexp_digest: V1.Ofday.t];
      [%expect {| 1893c2cb5f9a4f741e0751d13b2d812e |}]
    ;;

    let%expect_test "Interval.V1.Ofday_ns.t_sexp_grammar" =
      print_endline [%sexp_digest: V1.Ofday_ns.t];
      [%expect {| 62c867fbc256b26e42454ebee3d48b09 |}]
    ;;
  end)
;;

let%test_module "vs array" =
  (module struct
    open Interval.Int
    module Gen = Quickcheck.Generator

    let interval_of_length n =
      let open Gen.Let_syntax in
      if n = 0
      then Gen.singleton empty
      else (
        let range = n - 1 in
        let%map lo = Int.gen_incl Int.min_value (Int.max_value - range) in
        let hi = lo + range in
        create lo hi)
    ;;

    let interval =
      let open Gen.Let_syntax in
      let%bind n = Gen.small_non_negative_int in
      interval_of_length n
    ;;

    let interval_with_index =
      let open Gen.Let_syntax in
      let%bind n = Gen.small_non_negative_int in
      (* Can only generate indices for non-empty intervals. *)
      let n = n + 1 in
      let%bind index = Int.gen_incl 0 (n - 1) in
      let%map t = interval_of_length n in
      t, index
    ;;

    let interval_and_nearby_int =
      let open Gen.Let_syntax in
      let%bind n = Gen.small_non_negative_int in
      let%bind t = interval_of_length n in
      let%map nearby =
        if n = 0
        then Int.quickcheck_generator
        else (
          let lbound = lbound_exn t in
          let ubound = ubound_exn t in
          Int.gen_incl
            (if lbound - n <= lbound then lbound - n else Int.min_value)
            (if ubound + n >= ubound then ubound + n else Int.max_value))
      in
      t, nearby
    ;;

    type which =
      [ `Last_strictly_less_than
      | `Last_less_than_or_equal_to
      | `Last_equal_to
      | `First_equal_to
      | `First_greater_than_or_equal_to
      | `First_strictly_greater_than
      ]
    [@@deriving sexp_of]

    let which =
      Gen.of_list
        [ `Last_strictly_less_than
        ; `Last_less_than_or_equal_to
        ; `Last_equal_to
        ; `First_equal_to
        ; `First_greater_than_or_equal_to
        ; `First_strictly_greater_than
        ]
    ;;

    let%test_unit "to_list explicit" =
      let check lo hi list = [%test_eq: int list] (to_list (create lo hi)) list in
      check 0 5 [ 0; 1; 2; 3; 4; 5 ];
      check 0 0 [ 0 ];
      check 1 0 []
    ;;

    let%test_unit "to_list and to_array" =
      Quickcheck.test
        (let int = Int.gen_incl (-1000) 1000 in
         Gen.tuple2 int int)
        ~sexp_of:[%sexp_of: int * int]
        ~f:(fun (lo, hi) ->
          [%test_result: int list]
            ~expect:(List.range ~start:`inclusive ~stop:`inclusive lo hi)
            (to_list (create lo hi));
          [%test_eq: int list]
            (create lo hi |> to_list)
            (create lo hi |> to_array |> Array.to_list))
    ;;

    let%test_unit "to_list and to_array 2" =
      Quickcheck.test interval ~sexp_of:[%sexp_of: t] ~f:(fun t ->
        [%test_eq: int list] (to_list t) (to_array t |> Array.to_list))
    ;;

    let%test_unit "length" =
      Quickcheck.test interval ~sexp_of:[%sexp_of: t] ~f:(fun t ->
        [%test_result: int] ~expect:(Array.length (to_array t)) (length t))
    ;;

    let%test_unit "get" =
      Quickcheck.test interval_with_index ~sexp_of:[%sexp_of: t * int] ~f:(fun (t, i) ->
        [%test_result: int] ~expect:(to_array t).(i) (Private.get t i))
    ;;

    let%test_unit "iter" =
      Quickcheck.test interval ~sexp_of:[%sexp_of: t] ~f:(fun t ->
        [%test_result: int Queue.t]
          ~expect:
            (let q = Queue.create () in
             Array.iter (to_array t) ~f:(Queue.enqueue q);
             q)
          (let q = Queue.create () in
           iter t ~f:(Queue.enqueue q);
           q))
    ;;

    let%test_unit "fold" =
      Quickcheck.test interval ~sexp_of:[%sexp_of: t] ~f:(fun t ->
        let init = [] in
        let f xs x = x :: xs in
        [%test_result: int list]
          ~expect:(Array.fold (to_array t) ~init ~f)
          (fold t ~init ~f))
    ;;

    let%test_unit "min_elt w/ default compare" =
      Quickcheck.test interval ~sexp_of:[%sexp_of: t] ~f:(fun t ->
        [%test_result: int option]
          ~expect:(Array.min_elt (to_array t) ~compare:Int.compare)
          (min_elt t ~compare:Int.compare))
    ;;

    let%test_unit "min_elt w/ reverse compare" =
      Quickcheck.test interval ~sexp_of:[%sexp_of: t] ~f:(fun t ->
        let compare x y = Int.compare y x in
        [%test_result: int option]
          ~expect:(Array.min_elt (to_array t) ~compare)
          (min_elt t ~compare))
    ;;

    let%test_unit "max_elt w/ default compare" =
      Quickcheck.test interval ~sexp_of:[%sexp_of: t] ~f:(fun t ->
        [%test_result: int option]
          ~expect:(Array.max_elt (to_array t) ~compare:Int.compare)
          (max_elt t ~compare:Int.compare))
    ;;

    let%test_unit "max_elt w/ reverse compare" =
      Quickcheck.test interval ~sexp_of:[%sexp_of: t] ~f:(fun t ->
        let compare x y = Int.compare y x in
        [%test_result: int option]
          ~expect:(Array.max_elt (to_array t) ~compare)
          (max_elt t ~compare))
    ;;

    let%test_unit "mem" =
      Quickcheck.test
        interval_and_nearby_int
        ~sexp_of:[%sexp_of: t * int]
        ~f:(fun (t, i) ->
        [%test_result: bool] ~expect:(Array.mem ~equal:Int.equal (to_array t) i) (mem t i))
    ;;

    let%test_unit "binary_search" =
      Quickcheck.test
        Gen.(tuple2 interval_and_nearby_int which)
        ~sexp_of:[%sexp_of: (t * int) * which]
        ~f:(fun ((t, i), which) ->
          let array = to_array t in
          let compare = Int.compare in
          [%test_result: int option]
            ~expect:
              (Array.binary_search array ~compare which i
               |> Option.map ~f:(Array.get array))
            (binary_search t ~compare which i))
    ;;

    let%expect_test "explicit binary_search" =
      let pr x = print_endline @@ Sexp.to_string_hum @@ [%sexp_of: int option] x in
      pr
      @@ binary_search (create 4 80) ~compare:Int.compare `First_strictly_greater_than 18;
      [%expect {| (19) |}];
      pr
      @@ binary_search (create 25 80) ~compare:Int.compare `First_strictly_greater_than 18;
      [%expect {| (25) |}];
      pr
      @@ binary_search
           (create 25 80)
           ~compare:Int.compare
           `First_strictly_greater_than
           1000;
      [%expect {| () |}]
    ;;
  end)
;;

(* Tests for list bound functions *)
let%test_module _ =
  (module struct
    open Interval

    let intervals =
      [ Int.empty; Int.create 3 6; Int.create 2 7; Int.empty; Int.create 4 5 ]
    ;;

    let%expect_test _ =
      print_s [%sexp (Int.convex_hull intervals : Int.t)];
      [%expect {| (2 7) |}]
    ;;

    let intervals =
      [ Int.empty; Int.create 3 6; Int.create 2 3; Int.empty; Int.create 4 5 ]
    ;;

    let%expect_test _ =
      print_s [%sexp (Int.convex_hull intervals : Int.t)];
      [%expect {| (2 6) |}]
    ;;

    let intervals = [ Int.empty; Int.empty ]
    let%test _ = Int.is_empty (Int.convex_hull intervals)
  end)
;;

(* Tests for set functions *)
let%test_module _ =
  (module struct
    module Set = Interval.Set

    type t = int Set.t [@@deriving sexp]

    (** Turn an unsorted list of bounds into a sequence. First dedup, and then pair things
        up nicely.  If there's an odd number of elements, drop the last one. *)
    let make_intervals bounds =
      let[@tail_mod_cons] rec pair_up list =
        match list with
        | [] -> []
        | [ _ ] -> []
        | hd1 :: hd2 :: tl -> (hd1, hd2) :: pair_up tl
      in
      List.dedup_and_sort bounds ~compare:Int.compare |> pair_up
    ;;

    let%expect_test "make_intervals" =
      let test l = print_s [%sexp (make_intervals l : (int * int) list)] in
      test [];
      [%expect {| () |}];
      test [ 1; 3; 4 ];
      [%expect {| ((1 3)) |}];
      test [ 5; 1; 10; 11; 1; 7; 9; 2; 5 ];
      [%expect {|
        ((1 2)
         (5 7)
         (9 10))
        |}]
    ;;

    let quickcheck_generator =
      Quickcheck.Generator.map [%quickcheck.generator: int list] ~f:(fun bounds ->
        Set.create_exn (make_intervals bounds))
    ;;

    let%expect_test "Generated intervals sets are very likely both to contain and not \
                     contain random ints"
      =
      let gen = [%quickcheck.generator: t * int] in
      Quickcheck.test_can_generate gen ~trials:10 ~f:(fun (t, x) -> Set.contains t x);
      Quickcheck.test_can_generate gen ~trials:10 ~f:(fun (t, x) ->
        not (Set.contains t x))
    ;;

    (* As long as the tests are passing anyway, there's no reason to bother writing a
       sophisticated shrinker. *)
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic

    let%quick_test "union" =
      fun (t1 : t) (t2 : t) (xs : int list) ->
      let t3 = Set.union t1 t2 in
      List.iter xs ~f:(fun x ->
        [%test_result: bool]
          (Set.contains t3 x)
          ~expect:(Set.contains t1 x || Set.contains t2 x))
    ;;

    let%quick_test "intersect" =
      fun (t1 : t) (t2 : t) (xs : int list) ->
      let t3 = Set.inter t1 t2 in
      List.iter xs ~f:(fun x ->
        [%test_result: bool]
          (Set.contains t3 x)
          ~expect:(Set.contains t1 x && Set.contains t2 x))
    ;;

    let%quick_test "union_list" =
      fun (ts : t list) (xs : int list) ->
      let t = Set.union_list ts in
      List.iter xs ~f:(fun x ->
        [%test_result: bool]
          (Set.contains t x)
          ~expect:(List.exists ts ~f:(fun t -> Set.contains t x)))
    ;;

    let t1 = Set.create_exn [ 1, 5; 6, 10; 15, 20 ]
    let t2 = Set.create_exn [ 3, 12 ]

    let%expect_test "union demo" =
      print_s [%sexp (Set.union t1 t2 : int Set.t)];
      [%expect {|
        ((1  12)
         (15 20))
        |}]
    ;;

    let%expect_test "intersect demo" =
      print_s [%sexp (Set.inter t1 t2 : int Set.t)];
      [%expect {|
        ((3 5)
         (6 10))
        |}]
    ;;

    let%expect_test "union_list demo" =
      let ts = [ t1; t2; Set.create_exn [ 11, 13; 14, 15 ]; Set.create_exn [ 20, 22 ] ] in
      print_s [%sexp (Set.union_list ts : int Set.t)];
      [%expect {|
        ((1  13)
         (14 22))
        |}]
    ;;
  end)
;;
