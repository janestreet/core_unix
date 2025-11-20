(** Utility functions for dealing with the environment. *)

open! Core
open! Import
module Unix := Core_unix

(** [parse_ssh_client] reads an environment variable and retrieves the IP from which you
    are currently sshing. *)
val parse_ssh_client
  :  ?var_name:string (** default: [ssh_client_var_name] *)
  -> unit
  -> [ `From of Unix.Inet_addr.t | `Nowhere ] Or_error.t

(** The environment variable [ssh_client_var_name] will contain the IP address of the
    current SSH client.

    Example: A user is physically using host A. The user sshes from host A to host B and
    then from host B to host C. If checked on host C, the IP address will point to host B. *)
val ssh_client_var_name : string

(**/**)

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  val parse_ssh_client_var
    :  string option
    -> [ `From of Unix.Inet_addr.t | `Nowhere ] Or_error.t
end
