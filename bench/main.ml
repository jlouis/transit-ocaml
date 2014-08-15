open Core.Std
open Core_bench.Std
 
let base =
  In_channel.read_all "../transit-format/examples/0.8/example.json"
  
let decoded = Transit.from_string base

let () =
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"decode" 
      (fun () -> ignore (Transit.from_string base));
    Bench.Test.create ~name:"encode" 
      (fun () -> ignore (Transit.to_string decoded));
    Bench.Test.create ~name:"round_trip" 
      (fun () -> ignore (Transit.to_string (Transit.from_string base)))
  ])
