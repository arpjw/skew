open Core

let test_round_trip () =
  List.iter ["USD"; "EUR"; "GBP"; "JPY"; "CHF"] ~f:(fun s ->
    let c = Currency.of_string s in
    Alcotest.(check string) s s (Currency.to_string c))

let test_flip () =
  let p  = Currency.fx_pair Currency.USD Currency.EUR in
  let p2 = Currency.flip (Currency.flip p) in
  Alcotest.(check bool) "flip flip = id" true
    (Currency.equal_fx_pair p p2)

let test_other () =
  let c = Currency.of_string "XYZ" in
  Alcotest.(check string) "other" "XYZ" (Currency.to_string c)

let tests =
  [ "round_trip", `Quick, test_round_trip
  ; "flip",       `Quick, test_flip
  ; "other",      `Quick, test_other
  ]
