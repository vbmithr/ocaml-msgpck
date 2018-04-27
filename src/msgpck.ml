(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

module type STRING = sig
  type buf_in
  type buf_out

  val get_uint8 : buf_in -> int -> int
  val get_int8 : buf_in -> int -> int
  val get_uint16 : buf_in -> int -> int
  val get_int16 : buf_in -> int -> int
  val get_int32 : buf_in -> int -> int32
  val get_int64 : buf_in -> int -> int64
  val get_float : buf_in -> int -> float
  val get_double : buf_in -> int -> float

  val set_int8 : buf_out -> int -> int -> unit
  val set_int16 : buf_out -> int -> int -> unit
  val set_int32 : buf_out -> int -> int32 -> unit
  val set_int64 : buf_out -> int -> int64 -> unit
  val set_double : buf_out -> int -> float -> unit

  val length : buf_in -> int
  val blit : string -> int -> buf_out -> int -> int -> unit
  val sub : buf_in -> int -> int -> string
  val create_out : int -> buf_out
end

module SIBO = struct
  type buf_in = string
  type buf_out = Bytes.t

  include EndianString.BigEndian_unsafe

  let length = String.length
  let blit = Bytes.blit_string
  let sub = String.sub
  let create_out = Bytes.create
end

module BIBO = struct
  type buf_in = Bytes.t
  type buf_out = Bytes.t

  include EndianBytes.BigEndian_unsafe

  let length = Bytes.length
  let blit = Bytes.blit_string
  let sub = Bytes.sub_string
  let create_out = Bytes.create
end

module SIBUFO = struct
  type buf_in = string
  type buf_out = Buffer.t

  include EndianString.BigEndian_unsafe

  let scratch = Bytes.create 8

  let set_int8 buf _i i = set_int8 scratch 0 i; Buffer.add_subbytes buf scratch 0 1
  let set_int16 buf _i i = set_int16 scratch 0 i; Buffer.add_subbytes buf scratch 0 2
  let set_int32 buf _i i = set_int32 scratch 0 i; Buffer.add_subbytes buf scratch 0 4
  let set_int64 buf _i i = set_int64 scratch 0 i; Buffer.add_subbytes buf scratch 0 8
  let set_double buf _i f = set_double scratch 0 f; Buffer.add_subbytes buf scratch 0 8

  let length = String.length
  let blit i i_pos o _o_pos len = Buffer.add_substring o i i_pos len
  let sub = String.sub
  let create_out = Buffer.create
end

module BIBUFO = struct
  type buf_in = Bytes.t
  type buf_out = Buffer.t

  include EndianBytes.BigEndian_unsafe

  let scratch = Bytes.create 8

  let set_int8 buf _i i = set_int8 scratch 0 i; Buffer.add_subbytes buf scratch 0 1
  let set_int16 buf _i i = set_int16 scratch 0 i; Buffer.add_subbytes buf scratch 0 2
  let set_int32 buf _i i = set_int32 scratch 0 i; Buffer.add_subbytes buf scratch 0 4
  let set_int64 buf _i i = set_int64 scratch 0 i; Buffer.add_subbytes buf scratch 0 8
  let set_double buf _i f = set_double scratch 0 f; Buffer.add_subbytes buf scratch 0 8

  let length = Bytes.length
  let blit i i_pos o _o_pos len = Buffer.add_substring o i i_pos len
  let sub = Bytes.sub_string
  let create_out = Buffer.create
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
  | Map of (t * t) list

let compare = Pervasives.compare
let equal = Pervasives.(=)

let rec pp ppf t =
  let open Format in
  match t with
  | Nil -> pp_print_string ppf "()"
  | Bool b -> pp_print_bool ppf b
  | Int i -> pp_print_int ppf i
  | Uint32 i -> fprintf ppf "%ld" i
  | Int32 i -> fprintf ppf "%ld" i
  | Uint64 i -> fprintf ppf "%Ld" i
  | Int64 i -> fprintf ppf "%Ld" i
  | Float32 f -> pp_print_float ppf (Int32.to_float f)
  | Float f -> pp_print_float ppf f
  | String s -> pp_print_string ppf s
  | Bytes s -> fprintf ppf "%S" s
  | Ext (i, b) -> fprintf ppf "(%d %S)" i b
  | List ts ->
      let pp_sep ppf () = fprintf ppf ",@ " in
      fprintf ppf "[@[<hov 0>%a@]]" (pp_print_list ~pp_sep pp) ts
  | Map ts ->
      let pp_sep ppf () = fprintf ppf ",@ " in
      let pp_tuple ppf (k, v) =
        fprintf ppf "%a:@ %a" pp k pp v in
      fprintf ppf "{@[<hov 0>%a@]}" (pp_print_list ~pp_sep pp_tuple) ts

let show t =
  Format.asprintf "%a" pp t

let of_nil = Nil
let of_bool b = Bool b
let of_int i = Int i
let of_uint32 i = Uint32 i
let of_int32 i = Int32 i
let of_uint64 i = Uint64 i
let of_int64 i = Int64 i
let of_float32 i = Float32 i
let of_float f = Float f
let of_string s = String s
let of_bytes s = Bytes s
let of_ext t s = Ext (t, s)
let of_list l = List l
let of_map l = Map l

let raise_invalid_arg typ v =
  invalid_arg (Format.asprintf "to_%s: got %a" typ pp v)

let to_nil = function Nil -> () | v -> raise_invalid_arg "nil" v
let to_bool = function  Bool b -> b | v -> raise_invalid_arg "bool" v
let to_int = function Int i -> i | v -> raise_invalid_arg "int" v
let to_uint32 = function Uint32 i -> i | v -> raise_invalid_arg "uint32" v
let to_int32 = function Int32 i -> i | v -> raise_invalid_arg "int32" v
let to_uint64 = function Uint64 i -> i | v -> raise_invalid_arg "uint64" v
let to_int64 = function Int64 i -> i | v -> raise_invalid_arg "int64" v
let to_float32 = function Float32 f -> f | v -> raise_invalid_arg "float32" v
let to_float = function Float f -> f | v -> raise_invalid_arg "float" v
let to_string = function String s -> s | v -> raise_invalid_arg "string" v
let to_bytes = function Bytes b -> b | v -> raise_invalid_arg "bytes" v
let to_ext = function Ext (t, s) -> (t, s) | v -> raise_invalid_arg "ext" v
let to_list = function List l -> l | v -> raise_invalid_arg "list" v
let to_map = function Map l -> l | v -> raise_invalid_arg "map" v

module type S = sig
  type buf_in
  type buf_out

  val read : ?pos:int -> buf_in -> int * t
  val read_all : ?pos:int -> buf_in -> int * t list
  val size : t -> int
  val write : ?pos:int -> buf_out -> t -> int
  val to_string : t -> buf_out
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
  | i when i >= -0x1f - 1 -> set_int8 buf pos i; 1
  | i when i >= -0x7f - 1 -> set_int8 buf pos @@ 0xd0; set_int8 buf (pos+1) i; 2
  | i when i >= -0x7fff - 1 -> set_int8 buf pos 0xd1; set_int16 buf (pos+1) i; 3
  | i when i >= -0x7fff_ffff - 1 -> set_int8 buf pos 0xd2; set_int32 buf (pos+1) @@ Int32.of_int i; 5
  | i -> set_int8 buf pos 0xd3; set_int64 buf (pos+1) @@ Int64.of_int i; 9

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
  | Uint64 i -> write_uint64 ~pos buf i
  | Float32 i -> write_float ~pos buf i
  | Float f -> write_double ~pos buf f
  | String s -> write_string ~src:s ~dst_pos:pos ~dst:buf ()
  | Bytes s -> write_bin ~src:s ~dst_pos:pos ~dst:buf ()
  | Ext (t, d) -> write_ext ~src:d ~dst_pos:pos ~dst:buf t
  | List l -> begin
      let nb_written = match List.length l with
      | len when len <= 0xf -> set_int8 buf pos @@ 0x90 lor len; 1
      | len when len <= 0xffff -> set_int8 buf pos 0xdc; set_int16 buf (pos+1) len; 3
      | len -> set_int8 buf pos 0xdd; set_int32 buf (pos+1) (Int32.of_int len); 5
      in
      List.fold_left (fun nbw e -> nbw + write ~pos:(pos+nbw) buf e) nb_written l
    end
  | Map l -> begin
      let nb_written = match List.length l with
      | len when len <= 0xf -> set_int8 buf pos @@ 0x80 lor len; 1
      | len when len <= 0xffff -> set_int8 buf pos 0xde; set_int16 buf (pos+1) len; 3
      | len -> set_int8 buf pos 0xdf; set_int32 buf (pos+1) (Int32.of_int len); 5
      in
      List.fold_left begin fun nbw (k,v) ->
          let nbw = nbw + write ~pos:(pos+nbw) buf k in
          nbw + write ~pos:(pos+nbw) buf v
      end nb_written l
    end

  let size_int = function
  | i when i >= 0 && i <= 0x7f -> 1
  | i when i >= 0 && i <= 0xff -> 2
  | i when i >= 0 && i <= 0xffff -> 3
  | i when i >= 0 && i <= 0xffff_ffff -> 5
  | i when i >= -0x1f - 1 -> 1
  | i when i >= -0x7f - 1 -> 2
  | i when i >= -0x7fff - 1 -> 3
  | i when i >= -0x7fff_ffff - 1 -> 5
  | i -> 9

  let size_string str = match String.length str with
    | n when n <= 0x1f -> n+1
    | n when n <= 0xff -> n+2
    | n when n <= 0xffff -> n+3
    | n -> n+5

  let size_ext str = match String.length str with
    | 1 -> 1+2
    | 2 -> 2+2
    | 4 -> 4+2
    | 8 -> 8+2
    | 16 -> 16+2
    | n when n <= 0xff -> n+3
    | n when n <= 0xffff -> n+4
    | n -> n+6

  let rec size = function
  | Nil -> 1
  | Bool _ -> 1
  | Int i -> size_int i
  | Int32 _ | Uint32 _ | Float32 _ -> 5
  | Int64 _ | Uint64 _ | Float _ -> 9
  | String s | Bytes s -> size_string s
  | Ext (_typ, s) -> size_ext s
  | List l -> begin
      let nb_written = match List.length l with
      | len when len <= 0xf -> 1
      | len when len <= 0xffff -> 3
      | len -> 5
      in
      List.fold_left (fun nbw e -> nbw + size e) nb_written l
    end
  | Map l -> begin
      let nb_written = match List.length l with
      | len when len <= 0xf -> 1
      | len when len <= 0xffff -> 3
      | len -> 5
      in
      List.fold_left begin fun nbw (k,v) ->
          let nbw = nbw + size k in
          nbw + size v
      end nb_written l
    end

  let to_string msg =
    let buf = create_out @@ size msg in
    let _nb_written:int = write buf msg in
    buf

  let max_int31 = Int32.(shift_left one 30 |> pred)
  let min_int31 = Int32.(neg max_int31 |> pred)

  let max_int63 = Int64.(shift_left one 62 |> pred)
  let min_int63 = Int64.(neg max_int63 |> pred)

  let parse_int32 i = match Sys.word_size with
  | 32 -> if i >= min_int31 && i <= max_int31 then Int (Int32.to_int i) else Int32 i
  | 64 -> Int (Int32.to_int i)
  | _ -> invalid_arg "Sys.word_size"

  let parse_uint32 i = match Sys.word_size with
  | 32 -> if i >= 0l && i <= max_int31 then Int (Int32.to_int i) else Uint32 i
  | 64 -> Int (if i >= 0l then Int32.to_int i else 1 lsl 32 + Int32.to_int i)
  | _ -> invalid_arg "Sys.word_size"

  let parse_int64 i = match Sys.word_size with
  | 32 -> Int64 i
  | 64 -> if i >= min_int63 && i <= max_int63 then Int (Int64.to_int i) else Int64 i
  | _ -> invalid_arg "Sys.word_size"

  let parse_uint64 i = match Sys.word_size with
  | 32 -> Uint64 i
  | 64 -> if i >= 0L && i <= max_int63 then Int (Int64.to_int i) else Uint64 i
  | _ -> invalid_arg "Sys.word_size"

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
  | 0xce -> 5, parse_uint32 (get_int32 buf @@ pos+1)
  | 0xcf -> 9, parse_uint64 (get_int64 buf @@ pos+1)
  | 0xd0 -> 2, Int (get_int8 buf @@ pos+1)
  | 0xd1 -> 3, Int (get_int16 buf @@ pos+1)
  | 0xd2 -> 5, parse_int32 (get_int32 buf @@ pos+1)
  | 0xd3 -> 9, parse_int64 (get_int64 buf @@ pos+1)
  | 0xd4 -> 3, let typ = get_int8 buf (pos+1) in Ext (typ, (sub buf (pos+2) 1))
  | 0xd5 -> 4, let typ = get_int8 buf (pos+1) in Ext (typ, (sub buf (pos+2) 2))
  | 0xd6 -> 6, let typ = get_int8 buf (pos+1) in Ext (typ, (sub buf (pos+2) 4))
  | 0xd7 -> 10, let typ = get_int8 buf (pos+1) in Ext (typ, (sub buf (pos+2) 8))
  | 0xd8 -> 18, let typ = get_int8 buf (pos+1) in Ext (typ, (sub buf (pos+2) 16))
  | 0xd9 -> let len = get_uint8 buf (pos+1) in len+2, String (sub buf (pos+2) len)
  | 0xda -> let len = get_uint16 buf (pos+1) in len+3, String (sub buf (pos+3) len)
  | 0xdb -> let len = get_int32 buf (pos+1) |> Int32.to_int in len+5, String (sub buf (pos+5) len)
  | i when i >= 0xe0 -> 1, Int (get_int8 buf pos)
  | i -> invalid_arg (Printf.sprintf "read_one: unsupported tag 0x%x" i)

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

  let rec read_n ?(pos=0) buf n =
    let rec inner nbr elts n =
      if n > 0 then
        let nbr', elt = read ~pos:(pos+nbr) buf in
        inner (nbr+nbr') (elt::elts) (pred n)
      else nbr, elts
    in
    inner 0 [] n

  and read ?(pos=0) buf = match get_uint8 buf pos with
  | i when i lsr 4 = 0x8 -> let n = i land 0x0f in read_n ~pos:(pos+1) buf (2*n) |> fun (nb_read, elts) -> 1+nb_read, Map (pairs elts)
  | i when i lsr 4 = 0x9 -> let n = i land 0x0f in read_n ~pos:(pos+1) buf n |> fun (nb_read, elts) -> 1+nb_read, List (List.rev elts)
  | 0xdc -> let n = get_uint16 buf (pos+1) in read_n ~pos:(pos+3) buf n |> fun (nb_read, elts) -> 3+nb_read, List (List.rev elts)
  | 0xdd -> let n = get_uint32 buf (pos+1) in read_n ~pos:(pos+5) buf n |> fun (nb_read, elts) -> 5+nb_read, List (List.rev elts)
  | 0xde -> let n = get_uint16 buf (pos+1) in read_n ~pos:(pos+3) buf (2*n) |> fun (nb_read, elts) -> 3+nb_read, Map (pairs elts)
  | 0xdf -> let n = get_uint32 buf (pos+1) in read_n ~pos:(pos+5) buf (2*n) |> fun (nb_read, elts) -> 5+nb_read, Map (pairs elts)
  | _ -> read_one ~pos buf

  let read_all ?(pos=0) buf =
    let len = length buf in
    let rec inner acc pos =
      if pos >= len then pos, List.rev acc
      else
      let new_pos, msg = read ~pos buf in
      inner (msg :: acc) new_pos
    in
    inner [] pos
end

module String = Make(SIBO)
module Bytes = Make(BIBO)
module StringBuf = Make(SIBUFO)
module BytesBuf = Make(BIBUFO)

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
