module Skew_date = Date
open Core

let test_to_string () =
  let d = Skew_date.of_ymd 2000 1 1 in
  Alcotest.(check string) "2000-01-01" "2000-01-01" (Skew_date.to_string d)

let test_year_rollover () =
  let d  = Skew_date.of_ymd 2025 12 31 in
  let d' = Skew_date.add_days d 1 in
  Alcotest.(check (triple int int int)) "2026-01-01"
    (2026, 1, 1) (Skew_date.to_ymd d')

let test_diff_days () =
  let d1 = Skew_date.of_ymd 2026 1 1 in
  let d2 = Skew_date.of_ymd 2025 1 1 in
  Alcotest.(check int) "365" 365 (Skew_date.diff_days d1 d2)

let test_round_trip () =
  let dates =
    [ Skew_date.of_ymd 2020 2 29
    ; Skew_date.of_ymd 1999 12 31
    ; Skew_date.of_ymd 2000  1  1
    ; Skew_date.of_ymd 2024  3 15
    ]
  in
  List.iter dates ~f:(fun d ->
    let s  = Skew_date.to_string d in
    let d' = Skew_date.of_string s in
    Alcotest.(check int) ("round-trip " ^ s) d d')

let test_year_frac () =
  let d1 = Skew_date.of_ymd 2025 1 1 in
  let d2 = Skew_date.of_ymd 2026 1 1 in
  let yf = Skew_date.year_frac Skew_date.Act365 d1 d2 in
  Alcotest.(check (float 0.01)) "Act365 1yr" 1.0 yf

let tests =
  [ "to_string",    `Quick, test_to_string
  ; "year_rollover", `Quick, test_year_rollover
  ; "diff_days",    `Quick, test_diff_days
  ; "round_trip",   `Quick, test_round_trip
  ; "year_frac",    `Quick, test_year_frac
  ]
