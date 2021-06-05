(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Alcotest
module M = Msgpck
module Q = QCheck

let buf = Bytes.create (5 + (2 * 0x10000))
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

let reg_int_1 () =
  let m = M.(Int 4294967296) in
  let s = M.String.to_string m in
  let _, m2 = M.Bytes.read s in
  check msgpck "read int back" m m2

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
  checkb msgpck
    ~expected:(Int (Int32.to_int (-2147483647l)))
    (Bytes.of_string "\xd2\x80\x00\x00\x01") ;
  checkb msgpck
    ~expected:(Int (Int32.to_int (-2147483648l)))
    (Bytes.of_string "\xd3\xff\xff\xff\xff\x80\x00\x00\x00")

let negative_ints_32 () =
  checkb msgpck ~expected:(Int32 (-2147483647l))
    (Bytes.of_string "\xd2\x80\x00\x00\x01") ;
  checkb msgpck ~expected:(Int64 (-2147483648L))
    (Bytes.of_string "\xd3\xff\xff\xff\xff\x80\x00\x00\x00")

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
  let l32 = M.[(None, Int32 Int32.max_int); (None, Int 0x3fff_ffff)] in
  let l64 =
    M.
      [ (Some (Int Int32.(to_int max_int)), Int32 Int32.max_int)
      ; (Some (Int ~-1), Int32 0xffff_ffffl) ] in
  ListLabels.iter
    (if Sys.word_size = 32 then l32 else l64)
    ~f:(fun (expected, v) -> wr ?expected 5 v)

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

let overflow () =
  let msg = M.(List [Int 1; Int 2; Bool true]) in
  let s = M.Bytes.to_string msg in
  let b2 = Bytes.sub s 0 2 in
  let nbRead, v = M.Bytes.read s in
  check int "nbRead" 4 nbRead ;
  check msgpck "v" msg v ;
  check_raises "overflow" (Invalid_argument "index out of bounds") (fun () ->
      let _, _ = M.Bytes.read b2 in
      () )

let read_all () =
  let m0 = M.(List [Int 1; Int 2; Bool true]) in
  let b = M.Bytes.to_string m0 in
  check int "len" 4 (Bytes.length b) ;
  (let nr1, m1 = M.Bytes.read b in
   check int "nr" 4 nr1 ; check msgpck "m1" m0 m1 ) ;
  let b_long = Bytes.create 5 in
  Bytes.blit b 0 b_long 0 4 ;
  (let nr2, m2 = M.Bytes.read b_long in
   check int "nr2" 4 nr2 ;
   check msgpck "m2" m0 m2 ;
   let nr3, m3 = M.Bytes.read b_long ~pos:nr2 in
   check int "nr3" 1 nr3 ;
   check msgpck "m3" m3 (M.Int 0) ;
   let _, l = M.Bytes.read_all b_long in
   check (list msgpck) "read-all" l [m0; M.Int 0] ) ;
  ()

let basic =
  [ ("negative_ints", `Quick, negative_ints)
  ; ( "big_negative_ints"
    , `Quick
    , match Sys.word_size with 32 -> negative_ints_32 | _ -> negative_ints_64 )
  ; ("size1", `Quick, size1); ("size2", `Quick, size2); ("size3", `Quick, size3)
  ; ("size5", `Quick, size5); ("size9", `Quick, size9); ("str", `Quick, str)
  ; ("bytes", `Quick, bytes); ("bytes2", `Quick, bytes2); ("ext", `Quick, ext)
  ; ("array", `Quick, array); ("map", `Quick, map)
  ; ("rt", `Quick, match Sys.word_size with 32 -> rt32 | _ -> rt64)
  ; ("reg-int-1", `Quick, reg_int_1)
  ; ("read-all", `Quick, read_all); ("overflow", `Quick, overflow) ]

module Props = struct
  let gen_msg : M.t Q.Gen.t =
    let open Q.Gen in
    fix (fun self_sized depth ->
        frequency @@ List.flatten [
          [(2, let+ x = int in M.Int x);
           (1, let+ x = bool in M.Bool x);
           (2, let+ s = string_size (0 -- 50) in M.String s);
           (1, return M.of_nil);
           (1, let+ f = float in M.Float f);
           (1, let+ i = 0 -- 10 and+ s = string_size (0--10) in M.Ext (i,s));
          ];
          (if depth < 2
           then [
             (2, let+ l = list_size (0 -- (if depth=0 then 10 else 2)) (self_sized (depth+1)) in M.List l);
             (2, let+ l = list_size (0 -- (if depth=0 then 10 else 2))
                     (let+ k = string_size ~gen:printable (0 -- 5)
                      and+ v = self_sized (depth+1) in M.String k,v)
              in
              M.Map l);
           ] else []);
        ]) 0

  let rec shrink =
    let open Q.Iter in
    let module S = Q.Shrink in
    function
    | M.Int i -> let+ j = S.int i in M.Int j
    | M.Int32 i -> let+ i = S.int32 i in M.Int32 i
    | M.Int64 i -> let+ i = S.int64 i in M.Int64 i
    | M.Bool b -> if b then return (M.Bool false) else empty
    | M.String s -> let+ s = S.string s in M.String s
    | M.Bytes s -> let+ s = S.string s in M.String s
    | M.List l -> let+ l = S.list ~shrink l in M.List l
    | M.Map l -> let+ l = S.list ~shrink:(S.pair shrink shrink) l in M.Map l
    | M.Float _ | M.Uint32 _ | M.Uint64 _ | M.Float32 _ | M.Nil | M.Ext _ -> empty

  let arb_msg : M.t Q.arbitrary =
    Q.make ~shrink ~print:(fun m -> Format.asprintf "%a" M.pp m) gen_msg

  let pp_l out l = Format.fprintf out "[@[%a@]]"
      (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out ";@ ") M.pp) l

  let arb_msg_l : M.t list Q.arbitrary =
    Q.make ~shrink:(Q.Shrink.list ~shrink)
      ~print:(fun l -> Format.asprintf "%a" pp_l l)
      Q.Gen.(list_size (0 -- 10) gen_msg)
end

let ser_then_deser =
  QCheck_alcotest.to_alcotest ~verbose:true @@
  Q.Test.make ~count:1000 ~name:"ser-then-deser"
    Props.arb_msg
    (fun m ->
       let s = M.Bytes.to_string m in
       let off, m2 = M.Bytes.read s in
       m = m2 && off = Bytes.length s)

let ser_then_deser_l =
  QCheck_alcotest.to_alcotest ~verbose:true @@
  Q.Test.make ~count:1000 ~name:"ser-then-deser-list"
    Props.arb_msg_l
    (fun l ->
       let buf = Buffer.create 256 in
       let i = ref 0 in
       List.iter (fun m -> i := !i + M.BytesBuf.write ~pos:!i buf m) l;
       let s = Buffer.contents buf in
       let off, l2 = M.String.read_all s in
       l = l2 && off = Buffer.length buf)

(* test what happens when we serialize but then read only a prefix of the
   resulting byte array *)
let ser_then_deser_sub_list =
  QCheck_alcotest.to_alcotest ~verbose:true @@
  let rec list_take n l =
    match l with
    | _ when n <= 0 -> l
    | [] -> []
    | x :: tl -> x :: list_take (n-1) tl
  in
  let arb' =
    Q.(
      let gen =
        let open Q.Gen in
        let* l = list_size (1--15) Props.gen_msg in
        let s = Bytes.unsafe_to_string @@ M.String.to_string_all l in
        let+ i = 0 -- String.length s in
        l, s, i
    in
    let shrink (l,s,i) =
      let open Q.Iter in
      (* shrink [i] *)
      let s1 = let+ i = Q.Shrink.int i in (l,s,i) in
      (* shrink [l] and then adjust s and i *)
      let s2 =
        let+ l = Q.Shrink.list ~shrink:Props.shrink l in
        let s = Bytes.unsafe_to_string @@ M.String.to_string_all l in
        let i = min i (String.length s) in
        l, s, i
      in
      append s1 s2
    in
    let print (l,_s,i) = Format.asprintf "i=%d, l=%a" i Props.pp_l l in
    make ~shrink ~print gen)
  in
  Q.Test.make ~count:1000 ~name:"ser-then-deser-sub-list"
    arb'
    (fun (l,s,i) ->
       let sub = String.sub s 0 i in
       let _off, l2 = M.String.read_all ~allow_partial:true sub in
       if List.length l2 > List.length l then (
         Q.Test.fail_reportf "bad len:@ l=%a;@ l2=%a" Props.pp_l l Props.pp_l l2
       );
       let l' = list_take (List.length l2) l in
       if l <> l' then (
         Q.Test.fail_reportf "diff:@ l'=%a;@ l2=%a" Props.pp_l l' Props.pp_l l2
       );
       true)

let props =
  [ ser_then_deser
  ; ser_then_deser_l
  ; ser_then_deser_sub_list
  ]

let () = Alcotest.run "msgpck"
    [("basic", basic); ("prop", props)]

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
