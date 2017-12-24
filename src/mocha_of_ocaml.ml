type mocha_callback = (unit -> unit) Js.callback

module Binding = struct

  (* mocha's function that wrap up tests *)
  class type mocha = object
    method describe: Js.js_string Js.t -> (unit -> unit) Js.callback -> unit Js.meth

    (* mocha's function that include all tests *)
    method it: Js.js_string Js.t -> (unit -> unit) Js.callback -> unit Js.meth
    method it_async: Js.js_string Js.t -> (mocha_callback -> unit) Js.callback -> unit Js.meth
  end

  let mocha : mocha Js.t = Js.Unsafe.global

  class type assertion = object
    method ok: bool Js.t Js.meth
    method equal: 'a -> 'a -> unit Js.meth 
    method notEqual: 'a -> 'a -> unit Js.meth
    method deepStrictEqual: 'a -> 'a -> unit Js.meth
    method notDeepStrictEqual: 'a -> 'a -> unit Js.meth
    method fail: string -> unit Js.meth
  end

  let assertion = 
    let require = Js.Unsafe.pure_js_expr "require" in
    let module_ = Js.string "assert" in
    Js.Unsafe.(fun_call require [|inject module_|])
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
  | Ok v -> Binding.assertion##ok @@ Js.bool v
  | NotOk v -> Binding.assertion##ok @@ (not v |> Js.bool)
  | Fail v -> Binding.assertion##fail v

type test =
  | Sync of (string * (unit -> assertion))
  | Async of (string * (unit -> assertion Lwt.t))


let suite name tests =
  let name = Js.string name in
  let callback = Js.wrap_callback (fun () ->
      List.iter (function
          | Sync (name, f) ->
            let name = Js.string name in 
            let callback = Js.wrap_callback (fun () -> assertion_to_assert (f ())) in
            Binding.mocha##it name callback
          | Async (name, f) ->
            let name = Js.string name in 
            let callback = Js.wrap_callback (fun cb ->
                let open Lwt.Infix in
                let lwt = f () >|= (fun assertion ->
                    assertion_to_assert assertion
                  )
                  >>= (fun _ -> Js.Unsafe.fun_call cb [||] |> Lwt.return) in
                Lwt.ignore_result lwt
              ) in 
            Binding.mocha##it_async name callback
        )
        tests
    ) in
  Binding.mocha##describe name callback

let (>:::) = suite
let (>::) name cb = Sync (name, cb)
let (>:-) name cb = Async (name, cb)
