(lang dune 3.19)
(name skew)
(generate_opam_files true)
(package
 (name skew)
 (allow_empty)
 (depends
  (ocaml (>= 5.1.0))
  core
  core_unix
  ppx_jane
  alcotest))
