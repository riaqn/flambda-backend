(* TEST
   * expect
*)

(* Underscore partial application *)
let f ~(x : string) ~(y : int) (z:char) = ()
[%%expect{|
val f : x:string -> y:int -> char -> unit = <fun>
|}]

let g = f _
[%%expect{|
val g : char -> x:string -> y:int -> unit = <fun>
|}]

let g = f _ ~x:"hello"
[%%expect{|
val g : char -> y:int -> unit = <fun>
|}]

let g = f _ _
[%%expect{|
Line 1, characters 12-13:
1 | let g = f _ _
                ^
Error: The function applied to this argument has type
         x:string -> y:int -> unit
This argument cannot be applied without label
|}]

let g = f ~x:_
[%%expect{|
val g : x:string -> y:int -> char -> unit = <fun>
|}]

let g = f ~y:_
[%%expect{|
val g : y:int -> x:string -> char -> unit = <fun>
|}]

let g = f ~y:_ ~x:_
[%%expect{|
val g : x:string -> y:int -> char -> unit = <fun>
|}]

let g = f ~x:_ ~y:_
[%%expect{|
val g : x:string -> y:int -> char -> unit = <fun>
|}]

let g = f _ _ _
[%%expect{|
Line 1, characters 8-9:
1 | let g = f _ _ _
            ^
Warning 6 [labels-omitted]: labels x, y were omitted in the application of this function.
val g : x:string -> y:int -> char -> unit = <fun>
|}]

let f ?foo () = "hello"
[%%expect{|
val f : ?foo:'a -> unit -> string = <fun>
|}]

(* Due to our implementation, it's more natural to simply forbid underscore
   wrapped in Some *)
let g = f ~foo:_
[%%expect{|
Line 1, characters 15-16:
1 | let g = f ~foo:_
                   ^
Error: Underscore argument cannot be wrapped in Some.
|}]

(* Underscore can still be supplied as optional argument raw *)
let g = f ?foo:_
[%%expect{|
val g : ?foo:'_weak1 -> unit -> string = <fun>
|}]
