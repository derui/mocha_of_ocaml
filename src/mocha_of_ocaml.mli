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

(** Apply assertion to native. Use this function if want to extend this.  *)
val apply_assert: assertion -> unit

module Infix : sig
  val (<|>): assertion -> assertion -> assertion
end

(** Abstract testcase *)
module type Test = sig
  val run: unit -> unit
end

(* create suite *)
val suite : string -> (module Test) list -> unit
val (>:::): string -> (module Test) list -> unit

val (>::) : string -> (unit -> assertion) -> (module Test)

(* hooks. Functions named *_suite are once only per suite.
   Fuctions named *_each execute each case.
*)
val before_suite: (unit -> unit) -> (module Test)
val after_suite: (unit -> unit) -> (module Test)

val after_each: (unit -> unit) -> (module Test)
val before_each: (unit -> unit) -> (module Test)
