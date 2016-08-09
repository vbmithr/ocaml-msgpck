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
  | List of t list
  | Map of (t * t) list [@@deriving sexp]

module type S = sig
  type buf_in
  type buf_out

  val read : ?pos:int -> buf_in -> int * t
  val write : ?pos:int -> buf_out -> t -> int
end

module String : S with type buf_in = string and type buf_out = Bytes.t
module Bytes : S with type buf_in = Bytes.t and type buf_out = Bytes.t

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
