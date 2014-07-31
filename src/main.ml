open Core.Std

let () =
  let buf = In_channel.read_all "test.transit" in
  let tr1 = Transit.from_string buf in
  print_endline (if tr1 = `Null then "OK" else "FAIL")

