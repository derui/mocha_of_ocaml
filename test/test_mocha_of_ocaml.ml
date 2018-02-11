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
  ];

  (* Running test in JavaScript should be single thread, so this reference will thread safe. *)
  let value = ref 0 in
  "hooks test" >::: [
    before_suite (fun () -> Firebug.console##log "before suite");
    after_suite (fun () -> Firebug.console##log "after suite");

    before_each (fun () -> value := !value + 100);
    after_each (fun () -> value := 0);
    "can run hooks when before and after assertion" >:: (fun () -> assert_ok (!value = 100));
    "can run hooks on asynchronous assertion" >:- (fun () -> Lwt.return @@ assert_ok (!value = 100))
  ];

  "assertion combining" >::: [
    "should be able to use multiple assertion in one test" >:: (fun _ ->
        let open Infix in
        assert_ok (1 = 1) <|> assert_eq 2 2
      );
  ];
