type atime = Wf of float | W of int | D of int | Df of float
               
val add_time : int * int * int -> atime -> int * int * int
                                           
type months =
    January
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
  | December
    
exception IllegalMonthNum of int
    
val month_name : int -> string
val add_days : int * int * 'a -> int -> int * int * int
val add_weeks : int * int * int -> int -> int * int * int
val add_fdays : int * int * int -> float -> int * int * int
val add_fweeks : int * int * int -> float -> int * int * int
val print_date : int * int * int -> unit
val sprint_date : int * int * int -> string
