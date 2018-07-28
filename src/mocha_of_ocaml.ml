module Binding = struct

  (* mocha's function that wrap up tests *)
  class type mocha = object
    method describe: Js.js_string Js.t -> (unit -> unit) Js.callback -> unit Js.meth

    (* mocha's function that include all tests *)
    method it: Js.js_string Js.t -> (unit -> unit) Js.callback -> unit Js.meth
    method before: (unit -> unit) Js.callback -> unit Js.meth
    method beforeEach: (unit -> unit) Js.callback -> unit Js.meth
    method after: (unit -> unit) Js.callback -> unit Js.meth
    method afterEach: (unit -> unit) Js.callback -> unit Js.meth
  end

  let mocha : mocha Js.t = Js.Unsafe.global

  class type assertion = object
    method ok: bool Js.t -> unit Js.meth
    method equal: 'a -> 'a -> unit Js.meth
    method notEqual: 'a -> 'a -> unit Js.meth
    method deepStrictEqual: 'a -> 'a -> unit Js.meth
    method notDeepStrictEqual: 'a -> 'a -> unit Js.meth
    method fail: string -> unit Js.meth
  end

  let assertion : assertion Js.t = Js.Unsafe.js_expr "require('assert')"
end

type assertion =
  | Eq: 'a * 'a -> assertion
  | NotEq: 'a * 'a -> assertion
  | StrictEq: 'a * 'a -> assertion
  | NotStrictEq: 'a * 'a -> assertion
  | Ok: bool -> assertion
  | NotOk: bool -> assertion
  | Fail: string -> assertion
  | Combine: assertion * assertion -> assertion

let assert_ok v = Ok v
let assert_not_ok v = NotOk v
let assert_eq a b = Eq (a, b)
let assert_neq a b = NotEq (a, b)
let assert_strict_eq a b = StrictEq (a, b)
let assert_not_strict_eq a b = NotStrictEq (a, b)
let assert_fail message = Fail (message)

let rec apply_assert = function
  | Eq (a, b) -> Binding.assertion##equal a b
  | NotEq (a, b) -> Binding.assertion##notEqual a b
  | StrictEq (a, b) -> Binding.assertion##deepStrictEqual a b
  | NotStrictEq (a, b) -> Binding.assertion##notDeepStrictEqual a b
  | Ok v -> Binding.assertion##ok (Js.bool v)
  | NotOk v -> Binding.assertion##ok (not v |> Js.bool)
  | Fail v -> Binding.assertion##fail v
  | Combine (a1, a2) -> begin
      apply_assert a1;
      apply_assert a2
    end

module Infix = struct
  let (<|>) a1 a2 = Combine (a1, a2)
end

module type Test = sig
  val run: unit -> unit
end

let suite name tests =
  let name = Js.string name in
  let callback = Js.wrap_callback (fun () ->
      List.iter (fun (module M : Test) -> M.run ()) tests
    ) in
  Binding.mocha##describe name callback

let (>:::) = suite
let (>::) name cb =
  (module struct
    let run () =
      let name = Js.string name in
      let callback = Js.wrap_callback @@ fun () -> apply_assert (cb ()) in
      Binding.mocha##it name callback
  end : Test)

let before_suite cb =
  (module struct
    let run () = Binding.mocha##before (Js.wrap_callback cb)
  end : Test)
let before_each cb =
  (module struct
    let run () = Binding.mocha##beforeEach (Js.wrap_callback cb)
  end : Test)
let after_suite cb =
  (module struct
    let run () = Binding.mocha##after (Js.wrap_callback cb)
  end : Test)
let after_each cb =
  (module struct
    let run () = Binding.mocha##afterEach (Js.wrap_callback cb)
  end : Test)
