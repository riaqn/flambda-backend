(* TEST
   * expect
*)

(* This file tests various legacy constraints. *)

(* Tests about modules *)

module type S = sig val x : string end

module type Empty = sig end

module M = struct
    let x = "string"
end
[%%expect{|
module type S = sig val x : string end
module type Empty = sig end
module M : sig val x : string end
|}]

let (foo @ portable) () =
    let _ = (module M : S) in
    ()
[%%expect{|
Line 2, characters 20-21:
2 |     let _ = (module M : S) in
                        ^
Error: The module M is unportable, so cannot be used inside a closure that is portable.
|}]

(* File-level modules are looked up differently and need to be tested
separately. *)
let (foo @ portable) () =
    let _ = (module List : Empty) in
    ()
[%%expect{|
Line 2, characters 20-24:
2 |     let _ = (module List : Empty) in
                        ^^^^
Error: The module List is unportable, so cannot be used inside a closure that is portable.
|}]

let (foo @ portable) () = M.x
[%%expect{|
Line 1, characters 26-29:
1 | let (foo @ portable) () = M.x
                              ^^^
Error: The value M.x is unportable, so cannot be used inside a closure that is portable.
|}]

let (foo @ portable) () = List.length
[%%expect{|
Line 1, characters 26-37:
1 | let (foo @ portable) () = List.length
                              ^^^^^^^^^^^
Error: The value List.length is unportable, so cannot be used inside a closure that is portable.
|}]

let (foo @ portable) () =
    let m = (module struct let x = "hello" end : S) in
    let module M = (val m) in
    M.x
[%%expect{|
val foo : unit -> string = <fun>
|}]

(* Tests about classes *)
class cla = object
    method v  = "hello"
end
[%%expect{|
class cla : object method v : string end
|}]

let (foo @ portable) () =
    new cla
[%%expect{|
Line 2, characters 8-11:
2 |     new cla
            ^^^
Error: The class cla is unportable, so cannot be used inside a closure that is portable.
|}]

module type SC = sig
    class cla : object end
end
[%%expect{|
module type SC = sig class cla : object  end end
|}]

let (foo @ portable) () =
    let m = (module struct class cla = object end end : SC) in
    let module M = (val m) in
    new M.cla
[%%expect{|
val foo : unit -> <  > = <fun>
|}]

let obj @ portable = new cla
[%%expect{|
Line 1, characters 21-28:
1 | let obj @ portable = new cla
                         ^^^^^^^
Error: Found a unportable value where a portable value was expected
|}]

let foo @ portable =
    let obj = new cla in
    obj # v
[%%expect{|
Line 3, characters 4-11:
3 |     obj # v
        ^^^^^^^
Error: Found a unportable value where a portable value was expected
|}]
