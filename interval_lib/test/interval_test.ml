open Core
module Interval = Interval_lib.Interval

let ( @? ) name bool = if not bool then failwith name

let%expect_test "is_empty_or_singleton" =
  let t x = Interval.is_empty_or_singleton x in
  let i = Interval.create in
  "singleton1" @? t (i 0 0);
  "singleton2" @? t (i 10 10);
  "singleton3" @? t (i "foo" "foo");
  "empty1" @? t (i 1 0);
  "nonempty" @? not (t (i 0 1))
;;

let%expect_test "are_disjoint_as_open_intervals" =
  let t x = Interval.are_disjoint_as_open_intervals x in
  let i = Interval.create in
  "touching" @? t [ i 3 4; i 4 5 ];
  "not touching" @? t [ i 3 4; i 5 6 ];
  "overlapping" @? not (t [ i 3 5; i 4 6 ])
;;

let%expect_test "contains_set" =
  let module S = Interval.Set in
  let s1 = S.create_exn [ 1, 2; 3, 4; 5, 6 ] in
  let s2 = S.create_exn [ 3, 5; 10, 11 ] in
  let s3 = S.create_exn [ 3, 4 ] in
  "contains 1" @? S.contains s2 3;
  "contains 2" @? S.contains s2 4;
  "contains 3" @? not (S.contains s2 9);
  "contains 4" @? not (S.contains s2 12);
  "contains_set 1" @? not (S.contains_set ~container:s2 ~contained:s1);
  "contains_set 2" @? not (S.contains_set ~container:s1 ~contained:s2);
  "contains_set 3" @? S.contains_set ~container:s1 ~contained:s3;
  "contains_set 4" @? S.contains_set ~container:s2 ~contained:s3
;;

let%expect_test "half_open_intervals_are_a_partition" =
  "are_a_partition"
  @? Interval.half_open_intervals_are_a_partition
       [ Interval.create 0 2; Interval.create 2 4; Interval.create 4 8 ];
  "not_a_partition"
  @? not
       (Interval.half_open_intervals_are_a_partition
          [ Interval.create 0 2; Interval.create 2 4; Interval.create 5 8 ])
;;

let%expect_test "create_list_from_set" =
  let module Iv = Interval.Int in
  let test l =
    let l' =
      List.map ~f:(fun (x, y) -> Iv.create x y) l
      |> Iv.Set.create_from_intervals_exn
      |> Iv.Set.to_list
    in
    [%test_pred: Iv.t list] (List.is_sorted_strictly ~compare:Iv.compare) l';
    print_s [%sexp (l' : Iv.t list)]
  in
  test [ 1, 2; 23, 42; 3, 4; -4, -2 ];
  [%expect {| ((-4 -2) (1 2) (3 4) (23 42)) |}];
  test [];
  [%expect {| () |}];
  test [ 1, 10 ];
  [%expect {| ((1 10)) |}]
;;
