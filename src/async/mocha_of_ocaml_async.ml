open Mocha_of_ocaml

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

end

let (>:-) name cb =
  (module struct
    let run () =
      let name = Js.string name in
      let callback = Js.wrap_callback (fun () ->
          let open Lwt.Infix in
          let promise resolve _ =

            let lwt = Lwt_js.yield ()
              >>= cb
              >|= apply_assert
              >>= (fun _ -> Js.Unsafe.fun_call resolve [||] |> Lwt.return) in
            Lwt.ignore_result lwt
          in
          Binding.Promise.make promise
        ) in
      Binding.mocha##it_async name callback
  end : Test)
