(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** msgpack library for OCaml

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Msgpck} *)

type uint32
type uint64
type float32
type bstring

type _ t =
  | Nil : unit t
  | Bool : bool -> bool t
  | Int : int -> int t
  | Uint32 : int32 -> uint32 t
  | Int32 : int32 -> int32 t
  | Uint64 : int64 -> uint64 t
  | Int64 : int64 -> int64 t
  | Float32 : int32 -> float32 t
  | Float : float -> float t
  | String : string -> string t
  | Bytes : string -> bstring t
  | Ext : (int * string) -> (int * string) t
  | List : 'a t list -> 'a t list t
  | Map : ('a t * 'b t) list -> ('a t * 'b t) list t
  (** MessagePack types. *)

val pp : Format.formatter -> _ t -> unit
val show : _ t -> string

(** {1 Conversion functions OCaml -> MessagePack } *)

val nil : unit t
val of_bool : bool -> bool t
val of_int : int -> int t
val of_uint32 : uint32 -> int32 t
val of_int32 : int32 -> int32 t
val of_uint64 : uint64 -> int64 t
val of_int64 : int64 -> int64 t
val of_float32 : float32 -> int32 t
val of_float : float -> float t
val of_string : string -> string t
val of_bytes : string -> string t
val of_ext : int -> string -> string t
val of_list : 'a t list -> 'a t list t
val of_map : ('a t * 'b t) list -> ('a t * 'b t) list t

(** {1 Conversion functions MessagePack -> OCaml } *)

val to_nil : unit t -> unit
val to_bool : bool t -> bool
val to_int : int t -> int
val to_uint32 : uint32 t -> int32
val to_int32 : int32 t -> int32
val to_uint64 : uint64 t -> int64
val to_int64 : int64 t -> int64
val to_float32 : float32 t -> int32
val to_float : float t -> float
val to_string : string t -> string
val to_bytes : string t -> string
val to_ext : (int * string) t -> int * string
val to_list : 'a t list t -> 'a t list
val to_map : ('a t * 'b t) list t -> ('a t * 'b t) list

(** {1 Output signature for functors defined below } *)

module type S = sig
  type buf_in
  (** Type of input buffer (where MessagePack data will be read) *)
  type buf_out
  (** Type of output buffer (where MessagePack data will be
      written) *)

  val read : ?pos:int -> buf_in -> int * _ t
  (** [read ?pos buf] is [(nb_read, t)], where [nb_read] is the number
      of bytes read from [buf] at pos [?pos], and [t] is the decoded
      MessagePack value.

      [@raise] Invalid_argument "msg" when there is no valid
      MessagePack value to be read from [buf] at position [pos]. *)

  val size : _ t -> int
  (** [size msg] is the size in bytes of the MessagePack serialization
      of message [msg]. *)

  val write : ?pos:int -> buf_out -> _ t -> int
  (** [write ?pos buf msg] is [nb_written], the number of bytes
      written on [buf] at position [?pos]. The serialization of [msg]
      have been written to [buf] starting at [?pos]. *)

  val to_string : _ t -> buf_out
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
