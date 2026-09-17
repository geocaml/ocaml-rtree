(*
 * Copyright (c) 2019-2021 Craig Ferguson <craig@tarides.com>
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module List = struct
  include Stdlib.List

  let map f l =
    let rec aux acc = function
      | [] -> acc []
      | h :: t -> (aux [@tailcall]) (fun t' -> acc (f h :: t')) t
    in
    aux (fun x -> x) l
end

module Fmt = struct
  (* Part of this code is based on the fmt project and
     Copyright (c) 2020 The fmt programmers.
     SPDX-License-Identifier: ISC *)

  let list = Format.pp_print_list
  let sp ppf _ = Format.pp_print_space ppf ()

  let comma ppf _ =
    Format.pp_print_string ppf ",";
    sp ppf ()

  let pair pp_a pp_b ppf (a, b) =
    pp_a ppf a;
    comma ppf ();
    pp_b ppf b

  let triple pp_a pp_b pp_c ppf (a, b, c) =
    pp_a ppf a;
    comma ppf ();
    pp_b ppf b;
    comma ppf ();
    pp_c ppf c

  let surround s1 s2 pp_v ppf v =
    Format.(
      pp_print_string ppf s1;
      pp_v ppf v;
      pp_print_string ppf s2)

  let box ?(indent = 0) pp_v ppf v =
    Format.(
      pp_open_box ppf indent;
      pp_v ppf v;
      pp_close_box ppf ())

  let parens pp_v = box ~indent:1 (surround "(" ")" pp_v)
  let brackets pp_v = box ~indent:1 (surround "[" "]" pp_v)
  let oxford_brackets pp_v = box ~indent:1 (surround "[|" "|]" pp_v)
end
