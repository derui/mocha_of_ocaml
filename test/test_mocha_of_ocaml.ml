open Mocha_of_ocaml

let _ =
  "single test" >::: [
    "most simple test" >:: (fun _ -> assert_ok true)
  ];
  "multiple test" >::: [
    "ok" >:: (fun _ -> assert_ok (1 = 1));
    "not ok" >:: (fun _ -> assert_not_ok (1 <> 1));
    "eq" >:: (fun _ -> assert_eq "foo" "foo");
    "not eq" >:: (fun _ -> assert_neq "foo" "bar");
    "strict equal" >:: (fun _ -> assert_strict_eq "foo" "foo");
    "not strict equal" >:: (fun _ -> assert_not_strict_eq "foo" "bar");
  ];

  "async test" >::: [
    "set interval" >:- (
      fun () -> 
        let callback = Js.wrap_callback (fun () -> assert_ok true |> Lwt.return) in
        Js.Unsafe.call "setTimeout" Js.Unsafe.global [|Js.Unsafe.inject callback|]
    )
  ]
