module B = Bs_testing

let _ =
  B.suite "single test" [
      B.Sync ("most simple test", fun _ -> B.assert_ok true)
    ];
  B.suite "multiple test" [
      B.Sync ("ok", fun _ -> B.assert_ok (1 = 1));
      B.Sync ("not ok", fun _ -> B.assert_not_ok (1 <> 1));
      B.Sync ("eq", fun _ -> B.assert_eq "foo" "foo");
      B.Sync ("not eq", fun _ -> B.assert_neq "foo" "bar");
      B.Sync ("strict equal", fun _ -> B.assert_strict_eq "foo" "foo");
      B.Sync ("not strict equal", fun _ -> B.assert_not_strict_eq "foo" "bar");
    ];

  B.suite "async test" [
      B.Async ("set interval",
               fun () -> Js.Promise.make (fun ~resolve ~reject:_ ->
                             Js.Global.setTimeout (fun () -> resolve (B.assert_ok true) [@bs]) 1000 |> ignore
                           )
        )
    ]
