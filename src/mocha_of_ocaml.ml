type mocha_callback = (unit -> unit) Js.callback

module Binding = struct

  module Promise = struct

    type 'a resolve = ('a -> unit) Js.callback
    type 'a reject = ('a -> unit) Js.callback

    class type ['a, 'b] t = object
    end

    let make f =
      let ctor : (('a resolve -> 'b reject -> unit) Js.callback -> ('a, 'b) t Js.t) Js.constr = Js.Unsafe.global##.Promise in
      new%js ctor (Js.wrap_callback f)
  end

  (* mocha's function that wrap up tests *)
  class type mocha = object
    method describe: Js.js_string Js.t -> (unit -> unit) Js.callback -> unit Js.meth

    (* mocha's function that include all tests *)
    method it: Js.js_string Js.t -> (unit -> unit) Js.callback -> unit Js.meth
    method it_async: Js.js_string Js.t -> (unit -> ('a, 'b) Promise.t Js.t) Js.callback -> unit Js.meth
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

  let assertion = Js.Unsafe.js_expr "require('assert')"
end

type assertion =
  | Eq: 'a * 'a -> assertion
  | NotEq: 'a * 'a -> assertion
  | StrictEq: 'a * 'a -> assertion
  | NotStrictEq: 'a * 'a -> assertion
  | Ok: bool -> assertion
  | NotOk: bool -> assertion
  | Fail: string -> assertion

let assert_ok v = Ok v
let assert_not_ok v = NotOk v
let assert_eq a b = Eq (a, b)
let assert_neq a b = NotEq (a, b)
let assert_strict_eq a b = StrictEq (a, b)
let assert_not_strict_eq a b = NotStrictEq (a, b)
let assert_fail message = Fail (message)

let assertion_to_assert = function
  | Eq (a, b) -> Binding.assertion##equal a b
  | NotEq (a, b) -> Binding.assertion##notEqual a b
  | StrictEq (a, b) -> Binding.assertion##deepStrictEqual a b
  | NotStrictEq (a, b) -> Binding.assertion##notDeepStrictEqual a b
  | Ok v -> Binding.assertion##ok (Js.bool v)
  | NotOk v -> Binding.assertion##ok (not v |> Js.bool)
  | Fail v -> Binding.assertion##fail v

type test =
  | Sync of (string * (unit -> assertion))
  | Async of (string * (unit -> assertion Lwt.t))
  | Before_suite of (unit -> unit)
  | After_suite of (unit -> unit)
  | Before_each of (unit -> unit)
  | After_each of (unit -> unit)


let suite name tests =
  let name = Js.string name in
  let callback = Js.wrap_callback (fun () ->
      List.iter (function
          | Before_suite f -> Binding.mocha##before (Js.wrap_callback f)
          | Before_each f -> Binding.mocha##beforeEach (Js.wrap_callback f)
          | After_suite f -> Binding.mocha##after (Js.wrap_callback f)
          | After_each f -> Binding.mocha##afterEach (Js.wrap_callback f)
          | Sync (name, f) -> begin
              let name = Js.string name in
              let callback = Js.wrap_callback @@ fun () -> assertion_to_assert (f ()) in
              Binding.mocha##it name callback
            end
          | Async (name, f) -> begin
              let name = Js.string name in
              let callback = Js.wrap_callback (fun () ->
                  let open Lwt.Infix in
                  let promise resolve _ =

                    let lwt = Lwt_js.yield ()
                      >>= f
                      >|= assertion_to_assert
                      >>= (fun v -> Js.Unsafe.fun_call resolve [||] |> Lwt.return) in
                    Lwt.ignore_result lwt
                  in
                  Binding.Promise.make promise
                ) in
              Binding.mocha##it_async name callback
            end
        )
        tests
    ) in
  Binding.mocha##describe name callback

let (>:::) = suite
let (>::) name cb = Sync (name, cb)
let (>:-) name cb = Async (name, cb)

let before_suite cb = Before_suite cb
let before_each cb = Before_each cb
let after_suite cb = After_suite cb
let after_each cb = After_each cb
