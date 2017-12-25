open Mocha_of_ocaml

let _ =
  "single test" >::: [
    "most simple test" >:: (fun _ -> assert_ok true)
  ];
  "multiple test" >::: [
    "ok" >:: (fun _ -> assert_ok (1 = 1));
    "not ok" >:: (fun _ -> assert_not_ok (1 <> 1));
    "eq" >:: (fun _ -> assert_eq (Js.string "foo") (Js.string "foo"));
    "not eq" >:: (fun _ -> assert_neq (Js.string "foo") (Js.string "bar"));
    "strict equal" >:: (fun _ -> assert_strict_eq (Js.string "foo") (Js.string "foo"));
    "not strict equal" >:: (fun _ -> assert_not_strict_eq (Js.string "foo") (Js.string "bar"));
  ];
  
  "async test" >::: [
    "set interval" >:- (
      fun () -> 
        let callback = Js.wrap_callback (fun () -> assert_ok false |> Lwt.return) in

        Js.Unsafe.meth_call Js.Unsafe.global "setTimeout" [|
          Js.Unsafe.inject callback; Js.Unsafe.inject 0
        |]
    );
  ]
