open Swe

exception NotEnoughArgs

let get_date_and_print () =
  if (Array.length Sys.argv) < 3 then
    raise NotEnoughArgs
  else 
    let y = int_of_string Sys.argv.(1)
    and m = int_of_string Sys.argv.(2)
    and d = int_of_string Sys.argv.(3)
    in display_ephemeris_date (y,m,d);;

try
  get_date_and_print ()
with NotEnoughArgs -> (Printf.printf "Please supply y m d\n");;
