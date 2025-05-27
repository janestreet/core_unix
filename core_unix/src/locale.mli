open! Core
open! Import

(** Encapsulates a POSIX [locale_t], i.e. a set of language and cultural conventions for
    dealing with formatting of strings, numbers, dates, times, money, and so on.

    It cannot represent the special constants [LC_GLOBAL_LOCALE], representing the current
    global locale, nor [(locale_t)0], sometimes used to mean the current thread-local
    locale.

    Most of the operations can raise exceptions, but only in the case of misusing the API
    or a likely fatal situation such as out-of-memory, so the names are not explicitly
    suffixed with [_exn]. *)

module Category : sig
  (** A POSIX-standard locale category. An implementation may define additional locale
      categories beyond this list, but they are not representable with this type. *)
  type t =
    | Ctype
    (** Character classification and case conversion. Corresponds to POSIX [LC_CTYPE]. *)
    | Collate (** Collation order. Corresponds to POSIX [LC_COLLATE]. *)
    | Messages
    (** Formats of informative and diagnostic messages and interactive responses.
        Corresponds to POSIX [LC_MESSAGES]. *)
    | Monetary (** Monetary formatting. Corresponds to POSIX [LC_MONETARY]. *)
    | Numeric (** Numeric, non-monetary formatting. Corresponds to POSIX [LC_NUMERIC]. *)
    | Time (** Date and time formats. Corresponds to POSIX [LC_TIME]. *)
  [@@deriving compare ~localize, enumerate, equal ~localize, hash, sexp, string]

  val to_native : t -> int32
  val of_native : int32 -> t option
  val of_native_exn : int32 -> t
end

module Category_set : sig
  (** Represents a set of locale categories. This can include any of the standard
      categories that {!Category.t} defines, but also implementation-defined categories. *)
  type t [@@deriving sexp_of]

  include Flags.S with type t := t

  val of_category : Category.t -> t

  (** Returns a list of the standard categories included and the set of remaining
      non-standard categories. *)
  val to_category_list : t -> t * Category.t list

  val of_int : int -> t option
  val of_int_exn : int -> t
  val unsafe_of_int : int -> t
  val of_int32 : int32 -> t option
  val of_int32_exn : int32 -> t
  val unsafe_of_int32 : int32 -> t

  (** This conversion does not raise as long as an [unsafe_*] function has not been
      misused, as it follows from the C API that [int32] is sufficiently wide to hold all
      valid values. *)
  val to_int32 : t -> int32

  (** All supported categories. Includes all of those below but may include additional
      implementation-defined categories. Corresponds to POSIX [LC_ALL] and [LC_ALL_MASK]. *)
  val all : t

  (** All POSIX-standard categories, as listed in {!Category.t} and below. *)
  val standard : t

  (** Implementation-defined categories. Equivalent to [all - standard]. *)
  val nonstandard : t

  (** Character classification and case conversion. Corresponds to POSIX [LC_CTYPE] and
      [LC_CTYPE_MASK]. *)
  val ctype : t

  (** Collation order. Corresponds to POSIX [LC_COLLATE] and [LC_COLLATE_MASK]. *)
  val collate : t

  (** Formats of informative and diagnostic messages and interactive responses.
      Corresponds to POSIX [LC_MESSAGES] and [LC_MESSAGES_MASK]. *)
  val messages : t

  (** Monetary formatting. Corresponds to POSIX [LC_MONETARY] and [LC_MONETARY_MASK]. *)
  val monetary : t

  (** Numeric, non-monetary formatting. Corresponds to POSIX [LC_NUMERIC] and
      [LC_NUMERIC_MASK]. *)
  val numeric : t

  (** Date and time formats. Corresponds to POSIX [LC_TIME] and [LC_TIME_MASK]. *)
  val time : t
end

(** Constants for names of standard locales which always exist (in any category). *)
module Name : sig
  (** The "C" locale. This is a standardized minimal locale which is the default at
      program startup. *)
  val c : string

  (** The "POSIX" locale. Same as "C" on POSIX-compliant systems. *)
  val posix : string

  (** The empty string, indicating the native locale. This special value requests the
      locale be retrieved from the relevant [LC_*] or [LANG] environment variables where
      set, otherwise defaulting to an implementation-defined locale. *)
  val native : string
end

type t [@@deriving compare ~localize, equal ~localize, hash, sexp]

val ( = ) : t -> t -> bool
val ( <> ) : t -> t -> bool

include Hashable.S_plain with type t := t

(** A singleton locale created with {!Expert.posix} to avoid having to repeatedly
    construct it. As a result, the destructive operations like {!Expert.modify} and
    {!Expert.free} must not be called on this locale, as it would interfere with other
    users. *)
val posix : t Lazy.t

(** A singleton locale created with {!Expert.native} to avoid having to repeatedly
    construct it. As a result, the destructive operations like {!Expert.modify} and
    {!Expert.free} must not be called on this locale, as it would interfere with other
    users. *)
val native : t Lazy.t

(** Returns a string representation of the setting of a single category within a locale,
    in an implementation-defined format that is accepted by {!Expert.create}. *)
val to_string : t -> Category.t -> string

(** Returns a human-readable string of the settings of all of the POSIX-standard locale
    categories within a locale. *)
val to_string_hum : t -> string

(** Creates a {!t} using {!Expert.create}, calls the provided function with it, and frees
    it afterwards. Be careful when using it with a [Deferred.t]-returning function, as it
    would not wait on the [Deferred.t] before cleaning up. *)
val with_ : ?base:t -> ?category_mask:Category_set.t -> string -> (t -> 'a) -> 'a

(** Creates a {!t} using {!Expert.create_multi}, calls the provided function with it, and
    frees it afterwards. Be careful when using it with a [Deferred.t]-returning function,
    as it would not wait on the [Deferred.t] before cleaning up. *)
val with_multi : ?base:t -> (Category_set.t * string) list -> (t -> 'a) -> 'a

(** Creates a {!t} using {!Expert.copy}, calls the provided function with it, and frees it
    afterwards. Be careful when using it with a [Deferred.t]-returning function, as it
    would not wait on the [Deferred.t] before cleaning up. *)
val with_copy : t -> (t -> 'a) -> 'a

(** Returns the current thread-local locale, or [None] if the thread-local locale is
    dynamically tracking the global locale. This gives a direct reference rather than a
    snapshot. *)
val get_current : unit -> t option

(** Sets the current locale using {!Expert.set_current}, calls the provided function, and
    restores the current locale to its previous value afterwards. Be careful when using it
    with a [Deferred.t]-returning function, as it would not wait on the [Deferred.t]
    before cleaning up. *)
val with_current : t option -> (unit -> 'a) -> 'a

module Expert : sig
  (** Creates a {!t} using the "POSIX" locale (see {!Name.posix}) for all categories. The
      resulting {!t} should be freed with {!free} when it is no longer needed. Consider
      using the singleton {!Locale.posix} instead for efficiency. *)
  val posix : unit -> t

  (** Creates a {!t} using the native locale (see {!Name.native}) for all categories. The
      resulting {!t} should be freed with {!free} when it is no longer needed. Consider
      using the singleton {!Locale.native} instead for efficiency. *)
  val native : unit -> t

  (** Creates a copy of the current global locale. Note that the returned {!t} is a
      snapshot, so will not track subsequent changes to the global locale. It should be
      freed with {!free} when it is no longer needed. *)
  val global : unit -> t

  (** Creates a copy of the current thread-local locale (which defaults to the current
      global locale if not otherwise overridden). Note that the returned {!t} is a
      snapshot, so will not track subsequent changes to the current locale. It should be
      freed with {!free} when it is no longer needed. *)
  val current : unit -> t

  (** Creates a {!t} from a specified base locale (the default is the POSIX locale) and a
      set of categories (the default is all) to override with the given locale name. The
      resulting {!t} should be freed with {!free} when it is no longer needed. *)
  val create : ?base:t -> ?category_mask:Category_set.t -> string -> t

  (** Like {!create} but overrides multiple categories with potentially different locale
      names, with last-one-wins semantics in case of conflict. Equivalent to calling
      {!create} followed by {!modify} successively. *)
  val create_multi : ?base:t -> (Category_set.t * string) list -> t

  (** Creates a copy of a {!t}. This is useful in the case that destructive operations,
      such as {!modify} and {!free}, might be called on the original {!t}. The resulting
      {!t} should be freed with {!free} when it is no longer needed. *)
  val copy : t -> t

  (** Modifies a {!t} by overriding a set of categories (the default is all) with the
      given locale name. The passed {!t} is effectively freed, and so no further
      operations must be performed on it (including {!free} itself). The resulting {!t}
      should be freed with {!free} when it is no longer needed. *)
  val modify : t -> ?category_mask:Category_set.t -> string -> t

  (** Like {!modify} but overrides multiple categories with potentially different locale
      names, with last-one-wins semantics in case of conflict. Equivalent to calling
      {!modify} successively. *)
  val modify_multi : t -> (Category_set.t * string) list -> t

  (** Free a locale returned by {!posix}, {!native}, {!global}, {!current}, {!create},
      {!create_multi}, {!copy}, {!modify}, or {!modify_multi}. The locale must not be used
      subsequently. *)
  val free : t -> unit

  (** POSIX doesn't provide a standard way to set the global locale to a [locale_t], so
      you must instead specify a category to modify (default: all categories) and the name
      of the locale to modify it to. This is not guaranteed to be thread-safe in the case
      of other threads modifying or depending on the global locale, so it is recommended
      to use {!set_current} instead if possible. *)
  val set_global : ?category:Category.t -> string -> unit

  (** Sets the current thread-local locale. This is maintained as a direct reference to
      the locale rather than a copy, so [t] must not be freed until it's been replaced as
      the current locale. Returns the previous current locale. If [t] is [None], it
      results in the current locale dynamically tracking the global locale. The return
      value can also be [None], reflecting being in that state previously. *)
  val set_current : t option -> t option

  (** Returns the native [locale_t] value for use in POSIX C APIs. *)
  val to_native : t -> nativeint

  (** The native [locale_t] value [(locale_t)0]. *)
  val native_zero : nativeint

  (** The native [locale_t] value [LC_GLOBAL_LOCALE]. *)
  val native_global : nativeint
end
