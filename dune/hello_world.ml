let rec fib n = if n < 3 then 1
  else
    fib (n-1) + fib (n-2)


let () = Lwt_main.run (Lwt_io.print (string_of_int (fib 100)))

let zebra = print_endline "Hello World"; 5