(* TEST
   * expect *)

type r = {mutable a : string; b : string}

(* TESTING records *)

(* Reading/writing mutable field from contended record is rejected. Also note
    that the mutation error precedes type error. *)
let foo (r @ contended) = r.a <- 42
[%%expect{|
type r = { mutable a : string; b : string; }
Line 7, characters 26-27:
7 | let foo (r @ contended) = r.a <- 42
                              ^
Error: Found a contended value where a uncontended value was expected
|}]

let foo (r @ contended) = r.a
[%%expect{|
Line 1, characters 26-27:
1 | let foo (r @ contended) = r.a
                              ^
Error: Found a contended value where a uncontended value was expected
|}]

(* reading immutable field from contended record is fine *)
let foo (r @ contended) = r.b
[%%expect{|
val foo : r @ contended -> string @ contended = <fun>
|}]

(* Force top level to be uncontended and unportable *)
let r @ contended = "hello"
[%%expect{|
Line 1, characters 4-27:
1 | let r @ contended = "hello"
        ^^^^^^^^^^^^^^^^^^^^^^^
Error: Found a contended value where a uncontended value was expected
|}]

let x @ portable = "world"

let y @ portable = x
[%%expect{|
val x : string = "world"
Line 3, characters 19-20:
3 | let y @ portable = x
                       ^
Error: Found a unportable value where a portable value was expected
|}]

(* Closing over writing mutable field gives unportable *)
let foo () =
    let r = {a = "foo"; b = "bar"} in
    let bar () = r.a <- "hello" in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: Found a unportable value where a portable value was expected
|}]

(* Closing over reading mutable field gives unportable *)
let foo () =
    let r = {a = "foo"; b = "bar"} in
    let bar () = let _ = r.a in () in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: Found a unportable value where a portable value was expected
|}]

(* Closing over reading immutable field is OK *)
let foo () =
    let r @ portable = {a = "foo"; b = "bar"} in
    let bar () = let _ = r.b in () in
    let _ @ portable = bar in
    ()
(* CR zqian: currently mutable(legacy) means all records constructed are unportable,
   and the above bar is closing over an unportable record. Once we allow mutable()
   syntax, we can test this. *)
[%%expect{|
Line 2, characters 23-45:
2 |     let r @ portable = {a = "foo"; b = "bar"} in
                           ^^^^^^^^^^^^^^^^^^^^^^
Error: Found a unportable value where a portable value was expected
|}]


(* TESTING arrays *)
(* reading/writing to array requires uncontended *)
let foo (r @ contended) = Array.set r 42 "hello"
[%%expect{|
Line 1, characters 36-37:
1 | let foo (r @ contended) = Array.set r 42 "hello"
                                        ^
Error: Found a contended value where a uncontended value was expected
|}]
let foo (r @ contended) = Array.get r 42
[%%expect{|
Line 1, characters 36-37:
1 | let foo (r @ contended) = Array.get r 42
                                        ^
Error: Found a contended value where a uncontended value was expected
|}]
let foo (r @ contended) =
    match r with
    | [| x; y |] -> ()
[%%expect{|
Line 3, characters 6-16:
3 |     | [| x; y |] -> ()
          ^^^^^^^^^^
Error: Found a contended value where a uncontended value was expected
|}]


(* Closing over write gives unportable *)
let foo () =
    let r = [| "hello"; "world" |] in
    let bar () = Array.set r 0 "foo" in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: Found a unportable value where a portable value was expected
|}]

(* Closing over read gives unportable *)
let foo () =
    let r = [| "hello"; "world" |] in
    let bar () = Array.get r 0 in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: Found a unportable value where a portable value was expected
|}]


(* Closing over measuring length doesn't force unportable *)
let foo () =
    let r @ portable = [| "hello"; "world" |] in
    let bar () = Array.length r in
    let _ @ portable = bar in
    ()
(* CR zqian: The follows fails, because currently all records are constructed as
    unportable. Once we support mutable() syntax, we can test this. *)
[%%expect{|
Line 2, characters 23-45:
2 |     let r @ portable = [| "hello"; "world" |] in
                           ^^^^^^^^^^^^^^^^^^^^^^
Error: Found a unportable value where a portable value was expected
|}]


(* OTHER TESTS *)
(* Closing over uncontended but doesn't exploit that; the function is still
portable. *)
let foo () =
    let r @ portable uncontended = "hello" in
    let bar () = let _ = r in () in
    let _ @ portable = bar in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* Closing over unportable forces unportable. *)
let foo () =
    let r @ unportable = "hello" in
    let bar () = let _ = r in () in
    let _ @ portable = bar in
    ()
[%%expect{|
Line 4, characters 23-26:
4 |     let _ @ portable = bar in
                           ^^^
Error: Found a unportable value where a portable value was expected
|}]

let foo : 'a @ unportable contended -> ('a -> 'a) @ portable = fun a b -> "hello"
[%%expect{|
Line 1, characters 63-81:
1 | let foo : 'a @ unportable contended -> ('a -> 'a) @ portable = fun a b -> "hello"
                                                                   ^^^^^^^^^^^^^^^^^^
Error: This function when partially applied returns a unportable value,
       but expected to be portable.
|}]

let foo : 'a @ uncontended portable -> (string -> string) @ portable = fun a b -> "hello"
[%%expect{|
Line 1, characters 71-89:
1 | let foo : 'a @ uncontended portable -> (string -> string) @ portable = fun a b -> "hello"
                                                                           ^^^^^^^^^^^^^^^^^^
Error: This function when partially applied returns a unportable value,
       but expected to be portable.
|}]

let foo : 'a @ contended portable -> (string -> string) @ portable @@ unportable contended = fun a b -> "hello"
(* CR layout: arrows should cross contention. *)
[%%expect{|
Line 1, characters 4-111:
1 | let foo : 'a @ contended portable -> (string -> string) @ portable @@ unportable contended = fun a b -> "hello"
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Found a contended value where a uncontended value was expected
|}]

let foo : 'a @ contended portable -> (string -> string) @ portable @@ uncontended portable = fun a b -> "hello"
[%%expect{|
val foo : 'a @ portable contended -> (string -> string) @ portable = <fun>
|}]


(* immediates crosses portability and contention *)
let foo (x : int @@ unportable) (y : int @@ contended) =
    let _ @ portable = x in
    let _ @ uncontended = y in
    ()
[%%expect{|
val foo : int -> int @ contended -> unit = <fun>
|}]

(* TESTING immutable array *)
module Iarray = Stdlib__Iarray

let foo (r @ contended) = Iarray.get r 42
(* CR zqian: The following should pass. let's not change [iarray.mli] to accept
contended iarray; instead, let the modal kind system to mode cross iarray. *)
[%%expect{|
module Iarray = Stdlib__Iarray
Line 3, characters 37-38:
3 | let foo (r @ contended) = Iarray.get r 42
                                         ^
Error: Found a contended value where a uncontended value was expected
|}]



(* CR zqian: add portable/uncontended modality and test. *)
