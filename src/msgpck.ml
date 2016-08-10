(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Sexplib.Std

module type STRING = sig
  type buf_in
  type buf_out

  val get_char : buf_in -> int -> char
  val get_uint8 : buf_in -> int -> int
  val get_int8 : buf_in -> int -> int
  val get_uint16 : buf_in -> int -> int
  val get_int16 : buf_in -> int -> int
  val get_int32 : buf_in -> int -> int32
  val get_int64 : buf_in -> int -> int64
  val get_float : buf_in -> int -> float
  val get_double : buf_in -> int -> float
  val set_char : buf_out -> int -> char -> unit
  val set_int8 : buf_out -> int -> int -> unit
  val set_int16 : buf_out -> int -> int -> unit
  val set_int32 : buf_out -> int -> int32 -> unit
  val set_int64 : buf_out -> int -> int64 -> unit
  val set_float : buf_out -> int -> float -> unit
  val set_double : buf_out -> int -> float -> unit

  val blit : string -> int -> buf_out -> int -> int -> unit
  val sub : buf_in -> int -> int -> string
end

module BufString = struct
  type buf_in = string
  type buf_out = Bytes.t

  include EndianString.BigEndian_unsafe

  let blit = Bytes.blit_string
  let sub = String.sub
end

module BufBytes = struct
  type buf_in = Bytes.t
  type buf_out = Bytes.t

  include EndianBytes.BigEndian_unsafe

  let blit = Bytes.blit_string
  let sub = Bytes.sub_string
end

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
  | Map of (t * t) list [@@deriving sexp]

module type S = sig
  type buf_in
  type buf_out

  val read : ?pos:int -> buf_in -> int * t
  val write : ?pos:int -> buf_out -> t -> int
end

module Make (S : STRING) = struct
  include S

  let write_nil ?(pos=0) buf = set_int8 buf pos 0xc0; 1
  let write_bool ?(pos=0) buf b = set_int8 buf pos (if b then 0xc3 else 0xc2); 1
  let write_float ?(pos=0) buf i = set_int8 buf pos 0xca; set_int32 buf (pos+1) i; 5
  let write_double ?(pos=0) buf f = set_int8 buf pos 0xcb; set_double buf (pos+1) f; 9

  let write_int ?(pos=0) buf = function
  | i when i >= 0 && i <= 0x7f -> set_int8 buf pos i; 1
  | i when i >= 0 && i <= 0xff -> set_int16 buf pos (0xcc lsl 8 + i); 2
  | i when i >= 0 && i <= 0xffff -> set_int8 buf pos 0xcd; set_int16 buf (pos+1) i; 3
  | i when i >= 0 && i <= 0xffff_ffff -> set_int8 buf pos 0xce; set_int32 buf (pos+1) @@ Int32.of_int i; 5
  | i when i >= -0x1f -> set_int8 buf pos @@ 0xe0 lor (-i); 1
  | i when i >= -0x7f - 1 -> set_int16 buf pos @@ 0xd0 lsl 8 + (-i); 2
  | i when i >= -0x7fff - 1 -> set_int8 buf pos 0xd1; set_int16 buf (pos+1) (-i); 3
  | i when i >= -0x7fff_ffff - 1 -> set_int8 buf pos 0xd2; set_int32 buf (pos+1) Int32.(i |> of_int |> neg); 5
  | i -> set_int8 buf pos 0xd3; set_int64 buf (pos+1) @@ Int64.(i |> of_int |> neg); 9

  let write_uint32 ?(pos=0) buf i =
    set_int8 buf pos 0xce; set_int32 buf (pos+1) i; 5

  let write_uint64 ?(pos=0) buf i =
    set_int8 buf pos 0xcf; set_int64 buf (pos+1) i; 9

  let write_int32 ?(pos=0) buf i =
    set_int8 buf pos 0xd2; set_int32 buf (pos+1) i; 5

  let write_int64 ?(pos=0) buf i =
    set_int8 buf pos 0xd3; set_int64 buf (pos+1) i; 9

  let write_string ~src ?(src_pos=0) ~dst ?(dst_pos=0) ?src_len () =
    let len = match src_len with Some l -> l | None -> String.length src - src_pos in
    match len with
    | n when n <= 0x1f -> set_int8 dst dst_pos @@ 0xa0 lor n; blit src src_pos dst (dst_pos+1) len; len+1
    | n when n <= 0xff -> set_int16 dst dst_pos @@ 0xd9 lsl 8 + n; blit src src_pos dst (dst_pos+2) len; len+2
    | n when n <= 0xffff -> set_int8 dst dst_pos 0xda; set_int16 dst (dst_pos+1) len; blit src src_pos dst (dst_pos+3) len; len+3
    | _ -> set_int8 dst dst_pos 0xdb; set_int32 dst (dst_pos+1) (Int32.of_int len); blit src src_pos dst (dst_pos+5) len; len+5

  let write_bin ~src ?(src_pos=0) ~dst ?(dst_pos=0) ?src_len () =
    let len = match src_len with Some l -> l | None -> String.length src - src_pos in
    match len with
    | n when n <= 0xff -> set_int16 dst dst_pos @@ 0xc4 lsl 8 + n; blit src src_pos dst (dst_pos+2) len; len+2
    | n when n <= 0xffff -> set_int8 dst dst_pos 0xc5; set_int16 dst (dst_pos+1) len; blit src src_pos dst (dst_pos+3) len; len+3
    | _ -> set_int8 dst dst_pos 0xc6; set_int32 dst (dst_pos+1) (Int32.of_int len); blit src src_pos dst (dst_pos+5) len; len+5

  let write_ext ~src ?(src_pos=0) ~dst ?(dst_pos=0) ?src_len typ =
    let len = match src_len with Some l -> l | None -> String.length src - src_pos in
    match len with
    | 1 -> set_int16 dst dst_pos @@ 0xd4 lsl 8 + typ; blit src src_pos dst (dst_pos+2) len; len+2
    | 2 -> set_int16 dst dst_pos @@ 0xd5 lsl 8 + typ; blit src src_pos dst (dst_pos+2) len; len+2
    | 4 -> set_int16 dst dst_pos @@ 0xd6 lsl 8 + typ; blit src src_pos dst (dst_pos+2) len; len+2
    | 8 -> set_int16 dst dst_pos @@ 0xd7 lsl 8 + typ; blit src src_pos dst (dst_pos+2) len; len+2
    | 16 -> set_int16 dst dst_pos @@ 0xd8 lsl 8 + typ; blit src src_pos dst (dst_pos+2) len; len+2
    | n when n <= 0xff -> set_int8 dst dst_pos 0xc7; set_int16 dst (dst_pos+1) (n lsl 8 + typ); blit src src_pos dst (dst_pos+3) len; len+3
    | n when n <= 0xffff -> set_int32 dst dst_pos (0xc8 lsl 24 + len lsl 8 + typ |> Int32.of_int); blit src src_pos dst (dst_pos+4) len; len+4
    | _ -> set_int8 dst dst_pos 0xc9; set_int32 dst (dst_pos+1) (Int32.of_int len); set_int8 dst (dst_pos+5) typ; blit src src_pos dst (dst_pos+6) len; len+6

  let rec write ?(pos=0) buf = function
  | Nil -> write_nil ~pos buf
  | Bool b -> write_bool ~pos buf b
  | Int i -> write_int ~pos buf i
  | Int32 i -> write_int32 ~pos buf i
  | Uint32 i -> write_uint32 ~pos buf i
  | Int64 i -> write_int64 ~pos buf i
  | Uint64 i -> write_int64 ~pos buf i
  | Float32 i -> write_float ~pos buf i
  | Float f -> write_double ~pos buf f
  | String s -> write_string ~src:s ~src_pos:pos ~dst:buf ()
  | Bytes s -> write_bin ~src:s ~src_pos:pos ~dst:buf ()
  | Ext (t, d) -> write_ext ~src:d ~src_pos:pos ~dst:buf t
  | List l -> begin
      let init_pos = match List.length l with
      | len when len <= 0xf -> set_int8 buf pos @@ 0x90 lor len; 1
      | len when len <= 0xffff -> set_int8 buf pos 0xdc; set_int16 buf (pos+1) len; 3
      | len -> set_int8 buf pos 0xdd; set_int32 buf (pos+1) (Int32.of_int len); 5
      in
      let final_pos = List.fold_left (fun pos e -> pos + write ~pos buf e) init_pos l in
      final_pos - pos
    end
  | Map l -> begin
      let init_pos = match List.length l with
      | len when len <= 0xf -> set_int8 buf pos @@ 0x80 lor len; 1
      | len when len <= 0xffff -> set_int8 buf pos 0xde; set_int16 buf (pos+1) len; 3
      | len -> set_int8 buf pos 0xdf; set_int32 buf (pos+1) (Int32.of_int len); 5
      in
      let final_pos = List.fold_left (fun pos (k,v) -> let klen = write ~pos buf k in pos + klen + write ~pos:(pos+klen) buf v) init_pos l in
      final_pos - pos
    end

  let read_one ?(pos=0) buf = match get_uint8 buf pos with
  | i when i < 0x80 -> 1, Int (i land 0x7f)
  | i when i lsr 5 = 5 -> let len = (i land 0x1f) in succ len, String (sub buf (pos+1) len)
  | 0xc0 -> 1, Nil
  | 0xc2 -> 1, Bool false
  | 0xc3 -> 1, Bool true
  | 0xc4 -> let len = get_uint8 buf (pos+1) in len+2, Bytes (sub buf (pos+2) len)
  | 0xc5 -> let len = get_uint16 buf (pos+1) in len+3, Bytes (sub buf (pos+3) len)
  | 0xc6 -> let len = get_int32 buf (pos+1) |> Int32.to_int in len+5, Bytes (sub buf (pos+5) len)
  | 0xc7 -> let hdr = get_uint16 buf (pos+1) in let len = hdr lsr 8 in let typ = hdr land 0xff in len+3, Ext (typ, (sub buf (pos+3) len))
  | 0xc8 -> let len = get_uint16 buf (pos+1) in let typ = get_int8 buf (pos+3) in len+4, Ext (typ, (sub buf (pos+4) len))
  | 0xc9 -> let len = get_int32 buf (pos+1) |> Int32.to_int in let typ = get_int8 buf (pos+5) in len+6, Ext (typ, (sub buf (pos+6) len))
  | 0xca -> 5, Float (get_float buf @@ pos+1)
  | 0xcb -> 9, Float (get_double buf @@ pos+1)
  | 0xcc -> 2, Int (get_uint8 buf @@ pos+1)
  | 0xcd -> 3, Int (get_uint16 buf @@ pos+1)
  | 0xce -> 5, Uint32 (get_int32 buf @@ pos+1)
  | 0xcf -> 9, Uint64 (get_int64 buf @@ pos+1)
  | 0xd0 -> 2, Int (get_int8 buf @@ pos+1)
  | 0xd1 -> 3, Int (get_int16 buf @@ pos+1)
  | 0xd2 -> 5, Int32 (get_int32 buf @@ pos+1)
  | 0xd3 -> 9, Int64 (get_int64 buf @@ pos+1)
  | 0xd4 -> 3, let typ = get_int8 buf (pos+1) in Ext (typ, (sub buf (pos+2) 1))
  | 0xd5 -> 4, let typ = get_int8 buf (pos+1) in Ext (typ, (sub buf (pos+2) 2))
  | 0xd6 -> 6, let typ = get_int8 buf (pos+1) in Ext (typ, (sub buf (pos+2) 4))
  | 0xd7 -> 10, let typ = get_int8 buf (pos+1) in Ext (typ, (sub buf (pos+2) 8))
  | 0xd8 -> 18, let typ = get_int8 buf (pos+1) in Ext (typ, (sub buf (pos+2) 16))
  | 0xd9 -> let len = get_uint8 buf (pos+1) in len+2, String (sub buf (pos+2) len)
  | 0xda -> let len = get_uint16 buf (pos+1) in len+3, String (sub buf (pos+3) len)
  | 0xdb -> let len = get_int32 buf (pos+1) |> Int32.to_int in len+5, String (sub buf (pos+5) len)
  | i when i >= 0xe0 -> 1, Int (-(i land 0x1f))
  | i -> invalid_arg (Printf.sprintf "read_one: unsupported tag 0x%x" i)

  let read_n ?(pos=0) buf n =
    let rec inner cur_pos acc n =
      if n > 0 then
        let nb_read, elt = read_one ~pos:cur_pos buf in
        inner (cur_pos+nb_read) (elt::acc) (pred n)
      else cur_pos, acc
    in
    let final_pos, elts = inner pos [] n in
    final_pos - pos, elts

  let pairs l =
    List.fold_left begin fun acc e -> match acc with
    | None, acc -> Some e, acc
    | Some v, acc -> None, (e, v)::acc
    end (None, []) l |> snd

  let get_uint16 buf pos = match get_int16 buf pos with
  | i when i >= 0 -> i
  | i -> 0xffff + 1 + i

  let get_uint32 buf pos = match get_int32 buf pos |> Int32.to_int with
  | i when i >= 0 -> i
  | i -> 0xffff_ffff + 1 + i

  let rec read ?(pos=0) buf = match get_uint8 buf pos with
  | i when i lsr 4 = 0x8 -> let n = i land 0x0f in read_n ~pos:(pos+1) buf (2*n) |> fun (nb_read, elts) -> 1+nb_read, Map (pairs elts)
  | i when i lsr 4 = 0x9 -> let n = i land 0x0f in read_n ~pos:(pos+1) buf n |> fun (nb_read, elts) -> 1+nb_read, List (List.rev elts)
  | 0xdc -> let n = get_uint16 buf (pos+1) in read_n ~pos:(pos+3) buf n |> fun (nb_read, elts) -> 3+nb_read, List (List.rev elts)
  | 0xdd -> let n = get_uint32 buf (pos+1) in read_n ~pos:(pos+5) buf n |> fun (nb_read, elts) -> 5+nb_read, List (List.rev elts)
  | 0xde -> let n = get_uint16 buf (pos+1) in read_n ~pos:(pos+3) buf (2*n) |> fun (nb_read, elts) -> 3+nb_read, Map (pairs elts)
  | 0xdf -> let n = get_uint32 buf (pos+1) in read_n ~pos:(pos+5) buf (2*n) |> fun (nb_read, elts) -> 5+nb_read, Map (pairs elts)
  | _ -> read_one ~pos buf
end

module String = Make(BufString)
module Bytes = Make(BufBytes)

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
