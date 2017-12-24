# Helper for Unit test for js\_of\_ocaml on Mocha

This library provides helper functions that are base on Mocha as testing framework.

    Note: Assertion framework using in this library is as provided nodejs. So you should use bundler to run test in browser.

This inspired [bs-mocha](https://github.com/BuckleScript/bucklescript-addons/tree/master/bindings/bs-mocha).

* Asynchronous test

## Requirements ##

* Mocha

Each libraries must bind to ``global`` or ``window`` context. I recommends karma and karma-mocha when you run tests in browser.

## Example ##

```ocaml
open Mocha_of_ocaml

let _ = 

  "test suite" >::: [
    (* >:: operator create synchronized test *)
    "equal numbers" >:: (fun _ -> assert_eq 1 1);
    "greater than" >:: (fun _ -> assert_ok (1.0 < 1.5));
  ];
```

## Build ##

```
jbuilder build
```

## Test ##

```
jbuilder runtest
```

## License ##
MIT
