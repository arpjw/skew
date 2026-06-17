let test_mul_const () =
  let r = Dual.(var 3.0 *. const 2.0) in
  Alcotest.(check (float 1e-9)) "d/dx [2x] at x=3 = 2" 2.0 r.Dual.d

let test_mul_var () =
  (* d/dx [x*x] at x=3 = 2x = 6 *)
  let r = Dual.(var 3.0 *. var 3.0) in
  Alcotest.(check (float 1e-9)) "d/dx [x^2] at x=3 = 6" 6.0 r.Dual.d

let test_exp () =
  let r = Dual.exp (Dual.var 0.0) in
  Alcotest.(check (float 1e-9)) "exp(0).v = 1" 1.0 r.Dual.v;
  Alcotest.(check (float 1e-9)) "exp(0).d = 1" 1.0 r.Dual.d

let test_log () =
  let r = Dual.log (Dual.var 1.0) in
  Alcotest.(check (float 1e-9)) "log(1).v = 0" 0.0 r.Dual.v;
  Alcotest.(check (float 1e-9)) "log(1).d = 1" 1.0 r.Dual.d

let test_sqrt () =
  let r = Dual.sqrt (Dual.var 4.0) in
  Alcotest.(check (float 1e-9)) "sqrt(4).v = 2" 2.0 r.Dual.v;
  Alcotest.(check (float 1e-9)) "sqrt(4).d = 0.25" 0.25 r.Dual.d

let test_div () =
  let r = Dual.(const 1.0 /. var 2.0) in
  Alcotest.(check (float 1e-9)) "d/dx [1/x] at x=2 = -0.25" (-0.25) r.Dual.d

let test_add () =
  let r = Dual.(var 5.0 +. const 3.0) in
  Alcotest.(check (float 1e-9)) "add.v" 8.0 r.Dual.v;
  Alcotest.(check (float 1e-9)) "add.d" 1.0 r.Dual.d

let test_sub () =
  let r = Dual.(var 5.0 -. const 3.0) in
  Alcotest.(check (float 1e-9)) "sub.v" 2.0 r.Dual.v;
  Alcotest.(check (float 1e-9)) "sub.d" 1.0 r.Dual.d

let tests =
  [ "mul_const", `Quick, test_mul_const
  ; "mul_var",   `Quick, test_mul_var
  ; "exp",       `Quick, test_exp
  ; "log",       `Quick, test_log
  ; "sqrt",      `Quick, test_sqrt
  ; "div",       `Quick, test_div
  ; "add",       `Quick, test_add
  ; "sub",       `Quick, test_sub
  ]
