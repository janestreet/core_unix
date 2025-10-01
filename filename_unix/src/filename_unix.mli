@@ portable

(** Warning! this library assumes we are in a POSIX compliant OS. *)

open! Core
open! Import

(** [realpath path]
    @return the canonicalized absolute pathname of [path].
    @raise Unix_error on errors. *)
val realpath : string -> string

(** Same as {!temp_file}, but returns both the name of a fresh temporary file, and an
    output channel opened (atomically) on this file. This function is more secure than
    [temp_file]: there is no risk that the temporary file will be modified (e.g. replaced
    by a symbolic link) before the program opens it. *)
val open_temp_file
  :  ?close_on_exec:bool (** default true *)
  -> ?perm:int
  -> ?in_dir:string
  -> string
  -> string
  -> string * Out_channel.t

(** Similar to {!open_temp_file}, but returns a Unix file descriptor open in read&write
    mode instead of an [Out_channel.t]. *)
val open_temp_file_fd
  :  ?close_on_exec:bool (** default false *)
  -> ?perm:int
  -> ?in_dir:string
  -> string
  -> string
  -> string * Unix.file_descr

(** Creates an empty temporary file with a fresh name and returns the name of the
    temporary file. The temporary file is created in the directory specified by [in_dir],
    with permissions specified by [perm]. The base name of the temporary file is formed by
    concatenating [prefix], then ".tmp.", then 6 random alphanumeric characters, then
    [suffix]. The function ensures that the temporary filename does not already exist in
    the directory.

    @param perm
      the permission of the temporary file. The default value is [0o600] (readable and
      writable only by the file owner)

    @param in_dir
      the directory in which to create the temporary file. The default is [temp_dir_name]

      Note that prefix and suffix will be changed when necessary to make the final
      filename valid POSIX. *)
val temp_file : ?perm:int -> ?in_dir:string -> string -> string -> string

(** [temp_dir] is the same as [temp_file] but creates a temporary directory and uses a
    default permission value of [0o700] *)
val temp_dir : ?perm:int -> ?in_dir:string -> string -> string -> string

(** [create_arg_type]'s resulting [Arg_type.t] does bash autocompletion, via [compgen]. *)
val create_arg_type
  :  ?key:'a Univ_map.Multi.Key.t
  -> (string -> 'a)
  -> 'a Core.Command.Arg_type.t

(** [arg_type] is [create_arg_type Fn.id] *)
val arg_type : string Core.Command.Arg_type.t @@ nonportable
