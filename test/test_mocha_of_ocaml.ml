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
        let open Lwt.Infix in
        Lwt_js.sleep 0.0 >>= fun () -> assert_ok true |> Lwt.return
    );
  ]
