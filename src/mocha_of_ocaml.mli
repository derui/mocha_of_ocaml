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
val assertion_to_assert : assertion -> unit

(* type of testcase *)
type test

(* create suite *)
val suite : string -> test list -> unit
val (>:::): string -> test list -> unit

val (>::) : string -> (unit -> assertion) -> test
val (>:-) : string -> (unit -> assertion Lwt.t) -> test

