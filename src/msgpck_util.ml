(*---------------------------------------------------------------------------
   Copyright (c) 2017 Stavros Polymenis. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

exception Structural_mismatch of string
exception Type_mismatch of string
exception Value_mismatch of string

let rec string_of =
  let open Msgpck in
  function
  | String s-> s
  | Int i   -> string_of_int i
  | Bool b  -> string_of_bool b
  | Float f -> string_of_float f
  | List l  -> "[ " ^ List.fold_left (fun a e ->  a ^ string_of e ^ "; ") "" l ^ "]"
  | _ -> invalid_arg "unknown variant for string_of"

let to_bool   b = try Msgpck.to_bool   b with Invalid_argument e -> raise (Type_mismatch (e ^ " applied to " ^ string_of b))
let to_int    i = try Msgpck.to_int    i with Invalid_argument e -> raise (Type_mismatch (e ^ " applied to " ^ string_of i))
let to_float  f = try Msgpck.to_float  f with Invalid_argument e -> raise (Type_mismatch (e ^ " applied to " ^ string_of f))
let to_string s = try Msgpck.to_string s with Invalid_argument e -> raise (Type_mismatch (e ^ " applied to " ^ string_of s))
let to_list   l = try Msgpck.to_list   l with Invalid_argument e -> raise (Type_mismatch (e ^ " applied to " ^ string_of l))

let stub fn' b' = fn' b'
let b fn b = fn @@ to_bool   b
let i fn i = fn @@ to_int    i
let f fn f = fn @@ to_float  f
let s fn s = fn @@ to_string s
let l fn e = fn @@ to_list   e

let stubi fn idx x = fn idx x
let bi fn idx b = fn idx @@ to_bool   b
let ii fn idx i = fn idx @@ to_int    i
let fi fn idx f = fn idx @@ to_float  f
let si fn idx s = fn idx @@ to_string s
let li fn idx e = fn idx @@ to_list   e

let iter2 functions msgpck =
  let apply fn e = fn e in
  try List.iter2 apply functions @@ to_list msgpck
  with Invalid_argument s -> raise (Structural_mismatch ("iter2 failed at " ^ string_of msgpck))

let iter fn msgpck =
  let apply e = fn e in
  List.iter apply @@ to_list msgpck

let iteri fn msgpck =
  let apply i e = fn i e in
  List.iteri apply @@ to_list msgpck

let fold_left2 init fns msgpck =
  let apply acc fn e = fn acc e in
  List.fold_left2 apply init fns @@ to_list msgpck

let unimplemented a' = ()
let value v' a' = if v' <> a' then raise (Value_mismatch "unexpected msgpck value") else ()

let nth msg pos =
  try List.nth msg pos with
  | Failure s -> raise @@ Structural_mismatch ("Msgpck list too short; " ^ s)
  | Invalid_argument _ -> raise @@ Invalid_argument "Msgpck.nth"

let rec msg_of_buf ?(acc=(0,[])) buf =
  let (pos, elts) = acc in
  let (numof_bytes, msg) = Msgpck.StringBuf.read ~pos buf in
  if (numof_bytes + pos) < (String.length buf)
  then msg_of_buf ~acc:((numof_bytes + pos), (elts @ [msg])) buf
  else Msgpck.List (elts @ [msg])

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Stavros Polymenis

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
