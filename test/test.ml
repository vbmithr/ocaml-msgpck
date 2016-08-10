(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open OUnit2
module M = Msgpck

let buf = Bytes.create (5+2*0xffff)

let wr ?expected size v =
  let expected = match expected with Some v -> v | None -> v in
  let printer t = t |> M.sexp_of_t |> Sexplib.Sexp.to_string in
  let nb_written = M.Bytes.write buf v in
  assert_equal ~msg:"nb_written" ~printer:string_of_int size nb_written;
  let nb_read, msg = M.Bytes.read buf in
  assert_equal ~msg:"nb_read" ~printer:string_of_int size nb_read;
  assert_equal ~printer expected msg

let size1 ctx =
  let l = M.[Nil; Bool true; Bool false; Int 127; Int (-31)] in
  ListLabels.iter l ~f:(wr 1)

let size2 ctx =
  let l = M.[Int (-0x7f-1); Int 0xff] in
  ListLabels.iter l ~f:(wr 2)

let size3 ctx =
  let l = M.[Int (-0x7fff-1); Int 0xffff] in
  ListLabels.iter l ~f:(wr 3)

let size5 ctx =
  let l = M.[None, Int32 Int32.max_int; Some (Uint32 (-1l)), Int 0xffff_ffff] in
  ListLabels.iter l ~f:(fun (expected, v) -> wr ?expected 5 v)

let size9 ctx =
  let l = M.[None, Int64 Int64.max_int; None, Float 0.] in
  ListLabels.iter l ~f:(fun (expected, v) -> wr ?expected 9 v)

let str ctx =
  wr 5 (M.String "Bleh");
  wr (0x20+2) (M.String (Bytes.create 0x20 |> Bytes.unsafe_to_string));
  wr (0x100+3) (M.String (Bytes.create 0x100 |> Bytes.unsafe_to_string));
  wr (0x10000+5) (M.String (Bytes.create 0x10000 |> Bytes.unsafe_to_string))

let bytes ctx =
  wr (0x20+2) (M.Bytes (Bytes.create 0x20 |> Bytes.unsafe_to_string));
  wr (0x100+3) (M.Bytes (Bytes.create 0x100 |> Bytes.unsafe_to_string));
  wr (0x10000+5) (M.Bytes (Bytes.create 0x10000 |> Bytes.unsafe_to_string))

let ext ctx =
  wr 3 (M.Ext (4, "1"));
  wr 4 (M.Ext (4, "22"));
  wr 6 (M.Ext (4, "4444"));
  wr 10 (M.Ext (4, Bytes.create 8 |> Bytes.unsafe_to_string));
  wr 18 (M.Ext (4, Bytes.create 16 |> Bytes.unsafe_to_string));
  wr (0xff+3) (M.Ext (4, Bytes.create 0xff |> Bytes.unsafe_to_string));
  wr (0xffff+4) (M.Ext (4, Bytes.create 0xffff |> Bytes.unsafe_to_string))

let gen_list f n =
  let rec inner acc n =
    if n > 0 then inner ((f n)::acc) (pred n) else acc
  in inner [] n

let array ctx =
  wr (15+1) M.(List (gen_list (fun i -> Int i) 15));
  wr (0xffff+3) M.(List (gen_list (fun i -> Int 0) 0xffff));
  wr (0x10000+5) M.(List (gen_list (fun i -> Int 0) 0x10000))

let map ctx =
  wr (2*15+1) M.(Map (gen_list (fun i -> Int i, Int i) 15));
  wr (2*0xffff+3) M.(Map (gen_list (fun i -> Int 0, Int 0) 0xffff));
  wr (2*0x10000+5) M.(Map (gen_list (fun i -> Int 0, Int 0) 0x10000))

let suite =
  "msgpck" >::: [
    "size1" >:: size1;
    "size2" >:: size2;
    "size3" >:: size3;
    "size5" >:: size5;
    "size9" >:: size9;
    "str" >:: str;
    "bytes" >:: bytes;
    "ext" >:: ext;
    "array" >:: array;
    "map" >:: map;
  ]

let () = run_test_tt_main suite

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
