open Swe;;
  
type atime = Wf of float
           | W of int
           | D of int
           | Df of float;;

let add_time (y,m,d) time =
  let jd = day_to_jd (y,m,d)
  in let jd2 =
       match time with
         Wf(w) -> jd +. w *. 7.0
       | W(w) -> jd +. (float_of_int w) *. 7.0
       | D(d) -> jd +. (float_of_int d)
       | Df(d) -> jd +. d
  in jd_to_day jd2;;

type months =
  | January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December;;

exception IllegalMonthNum of int;;

let month_name n =
  match n with
    1 -> "January"
  | 2 -> "February"
  | 3 -> "March"
  | 4 -> "April"
  | 5 -> "May"
  | 6 -> "June"
  | 7 -> "July"
  | 8 -> "August"
  | 9 -> "September"
  | 10 -> "October"
  | 11 -> "November"
  | 12 -> "December"
  | _ -> raise (IllegalMonthNum n);;

let add_days (y,m,d) d = add_time (y,m,d) (D d);;
let add_weeks (y,m,d) w = add_time (y,m,d) (D w);;
let add_fdays (y,m,d) df = add_time (y,m,d) (Df df);;
let add_fweeks (y,m,d) wf = add_time (y,m,d) (Wf wf);;

let print_date (y,m,d) = Printf.printf "%u-%u-%u\n" y m d;;
let sprint_date (y,m,d) = Printf.sprintf "%u-%u-%u" y m d;;
