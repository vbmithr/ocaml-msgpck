(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Alcotest
module M = Msgpck

let buf = Bytes.create (5 + (2 * 0xffff))
let msgpck = testable M.pp M.equal

let roundtrip ?msg expected x =
  let buf = Buffer.create 13 in
  let _nbWritten = M.StringBuf.write buf x in
  let _posOut, x' = M.String.read (Buffer.contents buf) in
  let msg = "roundtrip" ^ match msg with None -> "" | Some msg -> ": " ^ msg in
  check msgpck msg expected x'

let max_int31 = Int.(shift_left one 30 |> pred)
let min_int31 = Int.(neg max_int31 |> pred)
let max_int31_32 = Int32.(shift_left one 30 |> pred)
let min_int31_32 = Int32.(neg max_int31_32 |> pred)
let max_int31_64 = Int64.(shift_left one 30 |> pred)
let min_int31_64 = Int64.(neg max_int31_64 |> pred)
let max_int63 = Int.(shift_left one 62 |> pred)
let min_int63 = Int.(neg max_int63 |> pred)
let max_int63_64 = Int64.(shift_left one 62 |> pred)
let min_int63_64 = Int64.(neg max_int63_64 |> pred)

let rt64 () =
  List.iter
    (fun (e, x) -> roundtrip e x)
    [ (Int 1, Int32 1l); (Int max_int31, Int max_int31)
    ; (Int max_int63, Int max_int63); (Int max_int63, Int64 max_int63_64)
    ; (Int min_int63, Int64 min_int63_64); (Uint64 (-1L), Uint64 (-1L)) ]

let rt32 () =
  List.iter
    (fun (e, x) -> roundtrip e x)
    [ (Int 1, Int32 1l); (Int max_int31, Int max_int31)
    ; (Int max_int31, Int max_int31) ]

let wr ?(section = "") ?expected size v =
  let expected = match expected with Some v -> v | None -> v in
  let nb_written = M.Bytes.write buf v in
  let computed_size = M.size v in
  check int (section ^ ": nb_written") size nb_written ;
  check int (section ^ ": size") nb_written computed_size ;
  let nb_read, msg = M.Bytes.read buf in
  check int (section ^ ": nb_read") size nb_read ;
  check msgpck (section ^ ": msgpck equality") expected msg

let checkb ?(msg = "") testable ~expected buf =
  let _nb_read, msgpck = M.Bytes.read buf in
  check testable msg expected msgpck

let negative_ints () =
  let open Bytes in
  checkb msgpck ~expected:(Int (-1)) (of_string "\xff") ;
  checkb msgpck ~expected:(Int (-33)) (of_string "\xd0\xdf") ;
  checkb msgpck ~expected:(Int (-32767)) (of_string "\xd1\x80\x01") ;
  checkb msgpck ~expected:(Int (-32768)) (of_string "\xd2\xff\xff\x80\x00")

let negative_ints_64 () =
  let open Bytes in
  checkb msgpck
    ~expected:(Int (Int32.to_int (-2147483647l)))
    (of_string "\xd2\x80\x00\x00\x01") ;
  checkb msgpck
    ~expected:(Int (Int32.to_int (-2147483648l)))
    (of_string "\xd3\xff\xff\xff\xff\x80\x00\x00\x00")

let negative_ints_32 () =
  let open Bytes in
  checkb msgpck ~expected:(Int32 (-2147483647l))
    (of_string "\xd2\x80\x00\x00\x01") ;
  checkb msgpck ~expected:(Int32 (-2147483648l))
    (of_string "\xd3\xff\xff\xff\xff\x80\x00\x00\x00")

let size1 () =
  let l =
    M.
      [ Nil; Bool true; Bool false; Int 127; Int (-32); Int (-31); Int (-30)
      ; Int (-2); Int (-1) ] in
  ListLabels.iter l ~f:(wr 1)

let size2 () =
  let l = M.[Int (-0x7f - 1); Int 0xff] in
  ListLabels.iter l ~f:(wr 2)

let size3 () =
  let l = M.[Int (-0x7fff - 1); Int 0xffff] in
  ListLabels.iter l ~f:(wr 3)

let size5 () =
  let l =
    M.
      [ (Some (Int Int32.(to_int max_int)), Int32 Int32.max_int)
      ; (None, Int 0xffff_ffff) ] in
  ListLabels.iter l ~f:(fun (expected, v) -> wr ?expected 5 v)

let size9 () =
  let l = M.[(None, Int64 Int64.max_int); (None, Float 0.)] in
  ListLabels.iter l ~f:(fun (expected, v) -> wr ?expected 9 v)

let str () =
  wr ~section:"empty string" 1 @@ M.String "" ;
  wr 5 (M.String "Bleh") ;
  wr (0x20 + 2) (M.String (Bytes.create 0x20 |> Bytes.unsafe_to_string)) ;
  wr (0x100 + 3) (M.String (Bytes.create 0x100 |> Bytes.unsafe_to_string)) ;
  wr (0x10000 + 5) (M.String (Bytes.create 0x10000 |> Bytes.unsafe_to_string))

let bytes () =
  wr (0x20 + 2) (M.Bytes (Bytes.create 0x20 |> Bytes.unsafe_to_string)) ;
  wr (0x100 + 3) (M.Bytes (Bytes.create 0x100 |> Bytes.unsafe_to_string)) ;
  wr (0x10000 + 5) (M.Bytes (Bytes.create 0x10000 |> Bytes.unsafe_to_string))

let bytes2 () =
  let msg = "my_payload is so really long tt" in
  let msgpck = M.of_bytes msg in
  let size = M.size msgpck in
  check int "bytes2: size" (String.length msg + 2) size ;
  let buf = Bytes.create size in
  let nb_written = M.Bytes.write buf msgpck in
  check int "bytes2: size written" size nb_written

let ext () =
  wr 3 (M.Ext (4, "1")) ;
  wr 4 (M.Ext (4, "22")) ;
  wr 6 (M.Ext (4, "4444")) ;
  wr 10 (M.Ext (4, Bytes.create 8 |> Bytes.unsafe_to_string)) ;
  wr 18 (M.Ext (4, Bytes.create 16 |> Bytes.unsafe_to_string)) ;
  wr (0xff + 3) (M.Ext (4, Bytes.create 0xff |> Bytes.unsafe_to_string)) ;
  wr (0xffff + 4) (M.Ext (4, Bytes.create 0xffff |> Bytes.unsafe_to_string))

let gen_list f n =
  let rec inner acc n = if n > 0 then inner (f n :: acc) (pred n) else acc in
  inner [] n

let array () =
  wr ~section:"empty list" 1 @@ M.List [] ;
  wr ~section:"one elt" 2 M.(List [Nil]) ;
  wr ~section:"small array" (15 + 1) M.(List (gen_list (fun i -> Int i) 15)) ;
  wr ~section:"medium array" (0xffff + 3)
    M.(List (gen_list (fun _ -> Int 0) 0xffff)) ;
  wr ~section:"large array" (0x10000 + 5)
    M.(List (gen_list (fun _ -> Int 0) 0x10000)) ;
  wr ~section:"concatenated lists" 2 M.(List [List []]) ;
  wr ~section:"string list" 2 M.(List [String ""]) ;
  wr ~section:"hello wamp" 33
    M.(
      List
        [Int 23; String "http://google.com"; Map [(String "subscriber", Map [])]])

let map () =
  wr ~section:"small map"
    ((2 * 15) + 1)
    M.(Map (gen_list (fun i -> (Int i, Int i)) 15)) ;
  wr ~section:"medium map"
    ((2 * 0xffff) + 3)
    M.(Map (gen_list (fun _ -> (Int 0, Int 0)) 0xffff)) ;
  wr ~section:"large map"
    ((2 * 0x10000) + 5)
    M.(Map (gen_list (fun _ -> (Int 0, Int 0)) 0x10000)) ;
  wr ~section:"concatenated maps" 3 M.(Map [(Nil, Map [])]) ;
  wr ~section:"string -> string" 3 M.(Map [(String "", String "")])

let basic =
  [ ("negative_ints", `Quick, negative_ints)
  ; ( "big_negative_ints"
    , `Quick
    , match Sys.word_size with 32 -> negative_ints_32 | _ -> negative_ints_64 )
  ; ("size1", `Quick, size1); ("size2", `Quick, size2); ("size3", `Quick, size3)
  ; ("size5", `Quick, size5); ("size9", `Quick, size9); ("str", `Quick, str)
  ; ("bytes", `Quick, bytes); ("bytes2", `Quick, bytes2); ("ext", `Quick, ext)
  ; ("array", `Quick, array); ("map", `Quick, map)
  ; ("rt", `Quick, match Sys.word_size with 32 -> rt32 | _ -> rt64) ]

let () = Alcotest.run "msgpck" [("basic,", basic)]

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
