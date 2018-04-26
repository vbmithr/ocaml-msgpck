(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

module Repr = struct
  type value = Msgpck.t

  let view : value -> value Json_repr.view = function
  | Msgpck.Nil -> `Null
  | Bool b -> `Bool b
  | Float f -> `Float f
  | String s -> `String s
  | Bytes b -> `String b
  | Int i -> `Float (float i)
  | Uint32 i -> `Float Int32.(to_float (if i < 0l then add i max_int else i))
  | Int32 i -> `Float (Int32.to_float i)
  | Uint64 i -> `Float Int64.(to_float (if i < 0L then add i max_int else i))
  | Int64 i -> `Float (Int64.to_float i)
  | Float32 i -> `Float (Int32.float_of_bits i)
  | Ext (i, b) -> `A [Int i ; String b]
  | List l -> `A l
  | Map m -> `A (List.map (fun (k, v) -> Msgpck.List [k ; v]) m)

  let repr : value Json_repr.view -> value = function
  | `A l -> List l
  | `Bool b -> Bool b
  | `Float f -> Float f
  | `Null -> Nil
  | `O kvs -> Map (List.map (fun (k, v) -> Msgpck.String k, v) kvs)
  | `String s -> String s

  let repr_uid = Json_repr.repr_uid ()
end

include Json_encoding.Make(Repr)

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
