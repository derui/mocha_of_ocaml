(* Type of assertion *)
type assertion

(* assertions *)
val assert_ok : bool -> assertion
val assert_not_ok : bool -> assertion
val assert_eq : 'a -> 'a -> assertion
val assert_neq : 'a -> 'a -> assertion
val assert_strict_eq : 'a -> 'a -> assertion
val assert_not_strict_eq : 'a -> 'a -> assertion
val assert_fail : string -> assertion

module Infix : sig
  val (<|>): assertion -> assertion -> assertion
end

(* type of testcase *)
type test

(* create suite *)
val suite : string -> test list -> unit
val (>:::): string -> test list -> unit

val (>::) : string -> (unit -> assertion) -> test
val (>:-) : string -> (unit -> assertion Lwt.t) -> test

(* hooks. Functions named *_suite are once only per suite.
   Fuctions named *_each execute each case.
*)
val before_suite: (unit -> unit) -> test
val after_suite: (unit -> unit) -> test

val after_each: (unit -> unit) -> test
val before_each: (unit -> unit) -> test
