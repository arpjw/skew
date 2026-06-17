let () =
  Date.today_provider := (fun () ->
    Int.of_float (Core_unix.gettimeofday () /. 86400.0));
  Repl.run ()
