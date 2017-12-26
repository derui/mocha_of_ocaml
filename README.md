# Helper for Unit test for js\_of\_ocaml on Mocha

This library provides helper functions that are base on Mocha as testing framework.

    Note: Assertion framework using in this library is as provided nodejs. So you should use bundler to run test in browser.

This inspired [bs-mocha](https://github.com/BuckleScript/bucklescript-addons/tree/master/bindings/bs-mocha) and [ocaml-webtest](https://github.com/johnelse/ocaml-webtest).

* Asynchronous test

## Requirements ##

* Mocha
* Lwt
* js\_of\_ocaml
* js\_of\_ocaml-lwt

Each libraries must bind to ``global`` or ``window`` context. I recommends karma and karma-mocha when you run tests in browser.

## Installation ##
1. Clone this repository
1. Add mocha\_of\_ocaml as pinned project

   ```shell
   $ cd path_to_mocha_of_ocaml
   $ opam pin add mocha_of_ocaml .
   $ opam install mocha_of_ocaml
   ```

1. Add mocha\_of\_ocaml to your project's dependency.

   If you already use opam, add ``depends`` follows.

   ```
   "mocha_of_ocaml" {"build" }
   ```


## Example ##

```ocaml
open Mocha_of_ocaml

let _ = 

  "test suite" >::: [
    (* >:: operator create synchronized test *)
    "equal numbers" >:: (fun () -> assert_eq 1 1);
    "greater than" >:: (fun () -> assert_ok (1.0 < 1.5));
  ]
  
  "including async tests"; >::: [
    (* >:- operator create asynchronized test *)
    "assert after sleep" >:- (fun () -> 
      let open Lwt.Infix in 
      Lwt_js.sleep 1.0 >>= (fun () -> assert_ok (1.0 < 1.5) |> Lwt.return)
    );
    "use promise returned from js function" >:- (fun () -> 
      let open Lwt.Infix in 
      let waiter,waker = Lwt.wait () in 
      let lwt = waiter >>= (fun () -> assert_ok true |> Lwt.return) in
      let promise = function_to_return_promise waker in 
      waiter
  ]
```

## Development ##

### Build ###

```
jbuilder build
```

### Test ###

```
jbuilder runtest
```

## License ##
MIT
