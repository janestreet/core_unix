open! Core
open! Import

module Category = struct
  external ctype : unit -> (int32[@unboxed]) = "unix_LC_CTYPE_bytecode" "unix_LC_CTYPE"

  external collate
    :  unit
    -> (int32[@unboxed])
    = "unix_LC_COLLATE_bytecode" "unix_LC_COLLATE"

  external messages
    :  unit
    -> (int32[@unboxed])
    = "unix_LC_MESSAGES_bytecode" "unix_LC_MESSAGES"

  external monetary
    :  unit
    -> (int32[@unboxed])
    = "unix_LC_MONETARY_bytecode" "unix_LC_MONETARY"

  external numeric
    :  unit
    -> (int32[@unboxed])
    = "unix_LC_NUMERIC_bytecode" "unix_LC_NUMERIC"

  external time : unit -> (int32[@unboxed]) = "unix_LC_TIME_bytecode" "unix_LC_TIME"

  let ctype = ctype ()
  let collate = collate ()
  let messages = messages ()
  let monetary = monetary ()
  let numeric = numeric ()
  let time = time ()

  type t =
    | Ctype
    | Collate
    | Messages
    | Monetary
    | Numeric
    | Time
  [@@deriving compare ~localize, enumerate, equal ~localize, hash, sexp, string]

  let to_native = function
    | Ctype -> ctype
    | Collate -> collate
    | Messages -> messages
    | Monetary -> monetary
    | Numeric -> numeric
    | Time -> time
  ;;

  let of_native =
    let open Int32.O in
    function
    | a when a = ctype -> Some Ctype
    | a when a = collate -> Some Collate
    | a when a = messages -> Some Messages
    | a when a = monetary -> Some Monetary
    | a when a = numeric -> Some Numeric
    | a when a = time -> Some Time
    | _ -> None
  ;;

  let of_native_exn =
    let open Int32.O in
    function
    | a when a = ctype -> Ctype
    | a when a = collate -> Collate
    | a when a = messages -> Messages
    | a when a = monetary -> Monetary
    | a when a = numeric -> Numeric
    | a when a = time -> Time
    | a -> failwithf "Locale.Category.of_native_exn: %ld" a ()
  ;;
end

module Category_set = struct
  external all
    :  unit
    -> (int32[@unboxed])
    = "unix_LC_ALL_MASK_bytecode" "unix_LC_ALL_MASK"

  external ctype
    :  unit
    -> (int32[@unboxed])
    = "unix_LC_CTYPE_MASK_bytecode" "unix_LC_CTYPE_MASK"

  external collate
    :  unit
    -> (int32[@unboxed])
    = "unix_LC_COLLATE_MASK_bytecode" "unix_LC_COLLATE_MASK"

  external messages
    :  unit
    -> (int32[@unboxed])
    = "unix_LC_MESSAGES_MASK_bytecode" "unix_LC_MESSAGES_MASK"

  external monetary
    :  unit
    -> (int32[@unboxed])
    = "unix_LC_MONETARY_MASK_bytecode" "unix_LC_MONETARY_MASK"

  external numeric
    :  unit
    -> (int32[@unboxed])
    = "unix_LC_NUMERIC_MASK_bytecode" "unix_LC_NUMERIC_MASK"

  external time
    :  unit
    -> (int32[@unboxed])
    = "unix_LC_TIME_MASK_bytecode" "unix_LC_TIME_MASK"

  let all = all () |> Int63.of_int32
  let ctype = ctype () |> Int63.of_int32
  let collate = collate () |> Int63.of_int32
  let messages = messages () |> Int63.of_int32
  let monetary = monetary () |> Int63.of_int32
  let numeric = numeric () |> Int63.of_int32
  let time = time () |> Int63.of_int32

  let known =
    [ ctype, "ctype"
    ; collate, "collate"
    ; messages, "messages"
    ; monetary, "monetary"
    ; numeric, "numeric"
    ; time, "time"
    ]
  ;;

  include%template Flags.Make (struct
      let known = known
      let remove_zero_flags = false
      let allow_intersecting = false
      let should_print_error = true
    end)

  let to_int32 = Int63.to_int32_trunc
  let unsafe_of_int = of_int

  let of_int a =
    let a = unsafe_of_int a in
    if is_subset ~of_:all a then Some a else None
  ;;

  let of_int_exn a =
    let a' = unsafe_of_int a in
    if is_subset ~of_:all a' then a' else failwithf "Locale.of_int_exn: %d" a ()
  ;;

  let unsafe_of_int32 = Int63.of_int32

  let of_int32 a =
    let a = unsafe_of_int32 a in
    if is_subset ~of_:all a then Some a else None
  ;;

  let of_int32_exn a =
    let a' = unsafe_of_int32 a in
    if is_subset ~of_:all a' then a' else failwithf "Locale.of_int_exn: %ld" a ()
  ;;

  let complement a = all - a
  let standard = List.fold known ~init:empty ~f:(fun acc (a, _) -> acc + a)
  let nonstandard = complement standard

  let of_category : Category.t -> _ = function
    | Ctype -> ctype
    | Collate -> collate
    | Messages -> messages
    | Monetary -> monetary
    | Numeric -> numeric
    | Time -> time
  ;;

  let to_category_list t =
    ( t - standard
    , List.filter_map Category.all ~f:(fun a ->
        if is_subset ~of_:t (of_category a) then Some a else None) )
  ;;
end

module Name = struct
  let c = "C"
  let posix = "POSIX"
  let native = ""
end

module T = struct
  type t = nativeint [@@deriving compare ~localize, equal ~localize, hash, sexp]
end

include T

include%template Comparable.Make [@mode portable] (T)
include%template Hashable.Make [@mode portable] (T)

external global_value
  :  unit
  -> (nativeint[@unboxed])
  = "unix_LC_GLOBAL_LOCALE_bytecode" "unix_LC_GLOBAL_LOCALE"

external zero_value
  :  unit
  -> (nativeint[@unboxed])
  = "unix_zero_locale_bytecode" "unix_zero_locale"

external all : unit -> (int32[@unboxed]) = "unix_LC_ALL_bytecode" "unix_LC_ALL"

external freelocale
  :  (nativeint[@unboxed])
  -> unit
  @@ portable
  = "unix_freelocale_bytecode" "unix_freelocale"

external duplocale
  :  (nativeint[@unboxed])
  -> (nativeint[@unboxed])
  @@ portable
  = "unix_duplocale_bytecode" "unix_duplocale"

external newlocale
  :  (int32[@unboxed])
  -> string
  -> (nativeint[@unboxed])
  -> (nativeint[@unboxed])
  @@ portable
  = "unix_newlocale_bytecode" "unix_newlocale"

external uselocale
  :  (nativeint[@unboxed])
  -> (nativeint[@unboxed])
  @@ portable
  = "unix_uselocale_bytecode" "unix_uselocale"

external getlocalename
  :  (int32[@unboxed])
  -> (nativeint[@unboxed])
  -> string
  = "unix_getlocalename_bytecode" "unix_getlocalename"

external setlocale
  :  (int32[@unboxed])
  -> string
  -> string
  = "unix_setlocale_bytecode" "unix_setlocale"

let global_value = global_value ()
let zero_value = zero_value ()
let all = all ()
let to_native = Fn.id
let free = freelocale
let to_string t category = getlocalename (Category.to_native category) t

let to_string_hum t =
  let names = List.map Category.all ~f:(fun category -> category, to_string t category) in
  match List.all_equal ~equal:String.equal (List.map names ~f:snd) with
  | Some name -> name
  | None ->
    names
    |> List.map ~f:(fun (category, name) -> Category.to_string category ^ ": " ^ name)
    |> String.concat ~sep:", "
;;

let copy = duplocale
let with_copy t f = Exn.protectx ~f (copy t) ~finally:free

let modify t ?(category_mask = Category_set.all) s =
  (* See comment in [create]. *)
  newlocale (Category_set.to_int32 category_mask) s t
;;

let modify_multi t l =
  List.fold l ~init:t ~f:(fun t (category_mask, s) -> modify t ~category_mask s)
;;

let create ?base ?(category_mask = Category_set.all) s =
  let base =
    match base with
    | None -> zero_value (* newlocale interprets a base of (locale_t)0 as POSIX. *)
    | Some base -> duplocale base
  in
  (* POSIX isn't completely clear on whether [newlocale] handles the degenerate case of a
     [category_mask] of 0, but we pass it on anyway. At least glibc does explicitly handle
     it. *)
  newlocale (Category_set.to_int32 category_mask) s base
;;

let with_ ?base ?category_mask s f =
  Exn.protectx ~f (create ?base ?category_mask s) ~finally:free
;;

let create_multi ?base = function
  | [] ->
    (match base with
     | None -> create Name.posix
     | Some base -> duplocale base)
  | (category_mask, s) :: tl -> modify_multi (create ?base ~category_mask s) tl
;;

let with_multi ?base l f = Exn.protectx ~f (create_multi ?base l) ~finally:free
let posix () = create Name.posix
let native () = create Name.native
let global () = duplocale global_value
let current () = duplocale (uselocale zero_value)

let set_global ?category s =
  let category = Option.value_map category ~f:Category.to_native ~default:all in
  ignore (setlocale category s : string)
;;

let get_current () =
  match uselocale zero_value with
  | t when t = global_value -> None
  | t -> Some t
;;

let set_current t =
  match uselocale (Option.value t ~default:global_value) with
  | t when t = global_value -> None
  | t -> Some t
;;

let with_current t f =
  let save = set_current t in
  Exn.protect ~f ~finally:(fun () -> ignore (set_current save : t option))
;;

module Expert = struct
  let posix = posix
  let native = native
  let global = global
  let current = current
  let create = create
  let create_multi = create_multi
  let to_native = to_native
  let copy = copy
  let modify = modify
  let modify_multi = modify_multi
  let free = free
  let set_global = set_global
  let set_current = set_current
  let native_zero = zero_value
  let native_global = global_value
end

module Portable = struct
  let posix = Portable_lazy.from_fun posix
  let native = Portable_lazy.from_fun native
end

let posix = lazy (posix ())
let native = lazy (native ())
