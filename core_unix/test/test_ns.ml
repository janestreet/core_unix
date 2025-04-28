open! Core
open! Import
open! Unix

let stats_match ~(base : stats) ~(ns : Ns_precision.stats) =
  let ts_match ftime ns_time =
    let diff =
      ftime -. (Time_ns.to_span_since_epoch ns_time |> Time_ns.Span.to_sec_approx)
    in
    Float.( < ) (Float.abs diff) 1e-6
  in
  [%compare.equal: int] base.st_dev ns.st_dev
  && [%compare.equal: int] base.st_ino ns.st_ino
  && Poly.( = ) base.st_kind ns.st_kind
  && Poly.( = ) base.st_perm ns.st_perm
  && [%compare.equal: int] base.st_nlink ns.st_nlink
  && [%compare.equal: int] base.st_uid ns.st_uid
  && [%compare.equal: int] base.st_gid ns.st_gid
  && [%compare.equal: int] base.st_rdev ns.st_rdev
  && [%compare.equal: int64] base.st_size ns.st_size
  && ts_match base.st_atime ns.st_atime
  && ts_match base.st_mtime ns.st_mtime
  && ts_match base.st_ctime ns.st_ctime
;;

let compare_stats ~base ~ns =
  if not (stats_match ~base ~ns)
  then raise_s [%message "Stat mismatch" (base : stats) (ns : Ns_precision.stats)]
;;

let dummy_time =
  Time_ns.of_span_since_epoch (Time_ns.Span.of_int_ns 123_456_789_123_456_789)
;;

let%expect_test "file stat matches" =
  let fname, fd = mkstemp "foo" in
  compare_stats ~base:(stat fname) ~ns:(Ns_precision.stat fname);
  compare_stats ~base:(lstat fname) ~ns:(Ns_precision.lstat fname);
  compare_stats ~base:(fstat fd) ~ns:(Ns_precision.fstat fd);
  close fd;
  unlink fname
;;

let%expect_test "dir stat matches" =
  let dname = mkdtemp "foo" in
  compare_stats ~base:(stat dname) ~ns:(Ns_precision.stat dname);
  compare_stats ~base:(lstat dname) ~ns:(Ns_precision.lstat dname);
  remove dname
;;

let%expect_test "link stat matches" =
  let fname, fd = mkstemp "foo" in
  let lname, lfd = mkstemp "foo2" in
  close fd;
  close lfd;
  unlink lname;
  symlink ~target:fname ~link_name:lname;
  compare_stats ~base:(stat lname) ~ns:(Ns_precision.stat lname);
  compare_stats ~base:(lstat lname) ~ns:(Ns_precision.lstat lname);
  unlink fname;
  unlink lname
;;

let%expect_test "ns precision roundtrips with utimensat" =
  let fname, fd = mkstemp "foo" in
  close fd;
  let original_stat = Ns_precision.stat fname in
  utimensat ~path:fname ~access:(Some dummy_time) ~modif:None ();
  [%test_result: Time_ns.t] (Ns_precision.stat fname).st_atime ~expect:dummy_time;
  [%test_result: Time_ns.t]
    (Ns_precision.stat fname).st_mtime
    ~expect:original_stat.st_mtime;
  utimensat ~path:fname ~access:(Some original_stat.st_atime) ~modif:(Some dummy_time) ();
  [%test_result: Time_ns.t]
    (Ns_precision.stat fname).st_atime
    ~expect:original_stat.st_atime;
  [%test_result: Time_ns.t] (Ns_precision.stat fname).st_mtime ~expect:dummy_time;
  unlink fname
;;

let rec sexp_replace_str ~from_str ~to_str = function
  | Sexp.Atom str when String.( = ) str from_str -> Sexp.Atom to_str
  | Atom _ as sexp -> sexp
  | List l -> List (List.map l ~f:(sexp_replace_str ~from_str ~to_str))
;;

let%expect_test "utimensat path resolution works as expected" =
  (* The behaviour we expect is:
     - If the path is absolute, [relative_to] is ignored
     - If the path is relative and [relative_to] is provided, it's relative to that dir
     - If the path is relative and [relative_to] is not provided, it's relative to cwd *)
  let fname, fd = mkstemp "foo" in
  let dirname = getcwd () in
  let dir_handle = opendir dirname in
  let dirfd = dirfd dir_handle in
  let set_and_assert_success fd ~relative_to ~path ~time =
    utimensat ~path ?relative_to ~access:(Some time) ~modif:(Some time) ();
    [%test_result: Time_ns.t] (Ns_precision.fstat fd).st_atime ~expect:time;
    [%test_result: Time_ns.t] (Ns_precision.fstat fd).st_mtime ~expect:time
  in
  let test fd ~path ~relative_to =
    (* Reset to ensure the test is effective *)
    set_and_assert_success
      fd
      ~path:[%string "/dev/fd/%{fd#File_descr}"]
      ~relative_to:None
      ~time:Time_ns.epoch;
    set_and_assert_success fd ~path ~relative_to ~time:dummy_time
  in
  chdir "/";
  test fd ~path:(dirname ^/ fname) ~relative_to:None;
  test fd ~path:(dirname ^/ fname) ~relative_to:(Some dirfd);
  test fd ~path:fname ~relative_to:(Some dirfd);
  (* Trying to access the file without relative_to fails because we're in the wrong cwd *)
  (match test fd ~path:fname ~relative_to:None with
   | exception exn ->
     Exn.sexp_of_t exn |> sexp_replace_str ~from_str:fname ~to_str:"FILENAME" |> print_s;
     [%expect {| (Unix.Unix_error "No such file or directory" utimensat FILENAME) |}]
   | () -> [%expect.unreachable]);
  chdir dirname;
  test fd ~path:fname ~relative_to:None;
  (* cleanup *)
  close dirfd;
  close fd;
  closedir dir_handle;
  unlink fname
;;

let%expect_test "utimensat link following" =
  let fname, fd = mkstemp "foo" in
  let lname, lfd = mkstemp "foo2" in
  close fd;
  close lfd;
  unlink lname;
  symlink ~target:fname ~link_name:lname;
  let link_original_stat = Ns_precision.lstat lname in
  utimensat ~path:lname ~access:None ~modif:(Some dummy_time) ();
  [%test_result: Time_ns.t] (Ns_precision.stat fname).st_mtime ~expect:dummy_time;
  [%test_result: Time_ns.t] (Ns_precision.stat lname).st_mtime ~expect:dummy_time;
  [%test_result: Time_ns.t]
    (Ns_precision.lstat lname).st_mtime
    ~expect:link_original_stat.st_mtime;
  utimensat ~path:lname ~access:None ~modif:(Some dummy_time) ~follow_symlinks:false ();
  [%test_result: Time_ns.t] (Ns_precision.lstat lname).st_mtime ~expect:dummy_time;
  unlink fname;
  unlink lname
;;

let%expect_test "utimensat error message" =
  require_does_raise (fun () ->
    utimensat
      ~path:"/nonexistent-file"
      ~access:(Some dummy_time)
      ~modif:(Some dummy_time)
      ());
  [%expect
    {| (Unix.Unix_error "No such file or directory" utimensat /nonexistent-file) |}]
;;
