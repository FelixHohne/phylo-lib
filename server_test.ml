open Core
open Async

let main () = 
  let f i = 
    printf "Value is %d\n" i;
    return () in 
  Deferred.both 
    (Deferred.List.iter [1;2;3;4;5] f)
    (Deferred.List.iter [1;2;3;4;5] f)

let () = 
  don't_wait_for (main () >>= fun _ -> exit 0);    
  ignore (Scheduler.go ())