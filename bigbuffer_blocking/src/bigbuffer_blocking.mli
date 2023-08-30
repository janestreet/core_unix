(** Extends {{!Core.Bigbuffer}[Core.Bigbuffer]}. *)

open! Core
open! Import
open! Core.Bigbuffer

(** [add_channel b ic n] reads exactly [n] characters from the input channel [ic] and
    stores them at the end of buffer [b].  Raises [End_of_file] if the channel contains
    fewer than [n] characters. *)
val add_channel : t -> In_channel.t -> int -> unit

(** [output_buffer oc b] writes the current contents of buffer [b] on the output channel
    [oc]. *)
val output_buffer : Out_channel.t -> t -> unit

(** Digest the current contents of the buffer. *)
val md5 : t -> Md5.t
