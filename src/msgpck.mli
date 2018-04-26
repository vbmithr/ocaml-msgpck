(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** msgpack library for OCaml

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Msgpck} *)

type t =
  | Nil
  | Bool of bool
  | Int of int
  | Uint32 of int32
  | Int32 of int32
  | Uint64 of int64
  | Int64 of int64
  | Float32 of int32
  | Float of float
  | String of string
  | Bytes of string
  | Ext of int * string
  | List of t list
  | Map of (t * t) list
  (** MessagePack types. *)

val compare : t -> t -> int
val equal : t -> t -> bool

val pp : Format.formatter -> t -> unit
val show : t -> string

(** {1 Conversion functions OCaml -> MessagePack } *)

val of_nil : t
val of_bool : bool -> t
val of_int : int -> t
val of_uint32 : int32 -> t
val of_int32 : int32 -> t
val of_uint64 : int64 -> t
val of_int64 : int64 -> t
val of_float32 : int32 -> t
val of_float : float -> t
val of_string : string -> t
val of_bytes : string -> t
val of_ext : int -> string -> t
val of_list : t list -> t
val of_map : (t * t) list -> t

(** {1 Conversion functions MessagePack -> OCaml } *)

val to_nil : t -> unit
val to_bool : t -> bool
val to_int : t -> int
val to_uint32 : t -> int32
val to_int32 : t -> int32
val to_uint64 : t -> int64
val to_int64 : t -> int64
val to_float32 : t -> int32
val to_float : t -> float
val to_string : t -> string
val to_bytes : t -> string
val to_ext : t -> int * string
val to_list : t -> t list
val to_map : t -> (t * t) list

(** {1 Output signature for functors defined below } *)

module type S = sig
  type buf_in
  (** Type of input buffer (where MessagePack data will be read) *)
  type buf_out
  (** Type of output buffer (where MessagePack data will be
      written) *)

  val read : ?pos:int -> buf_in -> int * t
  (** [read ?pos buf] is [(nb_read, t)], where [nb_read] is the number
      of bytes read from [buf] at pos [?pos], and [t] is the decoded
      MessagePack value.

      [@raise] Invalid_argument "msg" when there is no valid
      MessagePack value to be read from [buf] at position [pos]. *)

  val read_all : ?pos:int -> buf_in -> int * t list
  (** [read_all ?pos buf] reads all messages found in [buf].

      [@raise] Invalid_argument "msg" when there is no valid
      MessagePack value to be read from [buf] at position [pos]. *)

  val size : t -> int
  (** [size msg] is the size in bytes of the MessagePack serialization
      of message [msg]. *)

  val write : ?pos:int -> buf_out -> t -> int
  (** [write ?pos buf msg] is [nb_written], the number of bytes
      written on [buf] at position [?pos]. The serialization of [msg]
      have been written to [buf] starting at [?pos]. *)

  val to_string : t -> buf_out
  (** [to_string msg] is the MessagePack serialization of [msg]. *)
end

module StringBuf : S with type buf_in = string and type buf_out = Buffer.t
(** MessagePack library decoding from strings and writing in
    Buffers. *)

module BytesBuf : S with type buf_in = Bytes.t and type buf_out = Buffer.t
(** MessagePack library decoding from bytes and writing in Buffers. *)

module String : S with type buf_in = string and type buf_out = Bytes.t
(** MessagePack library decoding from strings and writing in bytes. *)

module Bytes : S with type buf_in = Bytes.t and type buf_out = Bytes.t
(** MessagePack library decoding from bytes and writing in bytes. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
