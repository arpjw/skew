let () =
  Alcotest.run "skew"
    [ "currency",   Test_currency.tests
    ; "date",       Test_date.tests
    ; "observable", Test_observable.tests
    ; "contract",   Test_contract.tests
    ; "checker",    Test_checker.tests
    ; "simplify",   Test_simplify.tests
    ; "market",     Test_market.tests
    ; "dual",       Test_dual.tests
    ; "path",       Test_path.tests
    ; "pricer",     Test_pricer.tests
    ; "greeks",     Test_greeks.tests
    ; "repl",       Test_repl.tests
    ]
