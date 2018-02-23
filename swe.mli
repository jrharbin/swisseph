type sw_latlong = {
  sw_long : float;
  sw_lat : float;
  sw_dist : float;
  sw_speedlong : float;
  sw_speedlat : float;
  sw_speeddist : float;
}

type planet =
    Ecl_nut
  | Sun
  | Moon
  | Mercury
  | Venus
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune
  | Pluto
  | Mean_node
  | True_node
  | Mean_apog
  | Oscu_apog
  | Earth
  | Chiron
  | Pholus
  | Ceres
  | Pallas
  | Juno
  | Vesta
  | Intp_apog
  | Intp_perg
  | Cupido
  | Hades
  | Zeus
  | Kronos
  | Apollon
  | Admetos
  | Vulkanus
  | Poseidon
  | Isis
  | Nibiru
  | Harrington
  | Neptune_leverrier
  | Neptune_adams
  | Pluto_lowell
  | Pluto_pickering
  | Vulcan
  | White_moon
  | Proserpina
  | Waldemath

type zodiac_sign =
  | Aries
  | Taurus
  | Gemini
  | Cancer
  | Leo
  | Virgo
  | Libra
  | Scorpio
  | Sagittarius
  | Capricorn
  | Aquarius
  | Pisces

type zodiac_sign_long= Zodiac of zodiac_sign * float;;

val planet_name : planet -> string
  
type coordinate_center = HELIO | GEO
type coordinate_system = POLAR | XYZ
type cal = CAL_GREG | CAL_JUL;;

exception SWEFailed
  
val planet_on_date :
  year:int ->
  month:int ->
  day:int ->
  ?hour:int ->
  ?min:int ->
  ?sec:float ->
  ?flag_override:int option ->
  ?center:coordinate_center ->
  ?cal:cal -> ?system:coordinate_system ->
  ?sw_results:float Ctypes.CArray.t ->
  planet -> sw_latlong

val helio_planet_on_date :
  year:int ->
  month:int ->
  day:int ->
  ?hour:int ->
  ?min:int ->
  ?sec:float ->
  ?flag_override:int option ->
  ?cal:cal -> ?system:coordinate_system ->
  ?sw_results:float Ctypes.CArray.t ->
  planet -> sw_latlong
  
val geo_planet_on_date :
  year:int ->
  month:int ->
  day:int ->
  ?hour:int ->
  ?min:int ->
  ?sec:float ->
  ?flag_override:int option ->
  ?cal:cal -> ?system:coordinate_system ->
  ?sw_results:float Ctypes.CArray.t ->
  planet -> sw_latlong

val day_to_jd : int * int * int -> float
val jd_to_day : float -> int * int * int
val day_diff : int * int * int -> int * int * int -> float
val zodiac_str : ?short_sign:bool-> zodiac_sign_long -> string
val zodiac_string : ?short_sign:bool-> sw_latlong -> string

val find_matching_jds_planet :
  (sw_latlong -> bool) ->
  jd1:float ->
  jd2:float ->
  ?flag_override:int option ->
  ?center:coordinate_center ->
  ?cal:cal -> ?system:coordinate_system -> planet -> float list

val find_matching_jds_planets2 :
  ?planet2_offset_jd:float ->
  (sw_latlong -> sw_latlong -> bool) ->
  jd1:float ->
  jd2:float ->
  ?flag_override:'a option ->
  ?center:coordinate_center ->
  ?cal:cal -> ?system:coordinate_system -> planet -> planet -> float list

val load_library_path : string -> unit;;
val load_library_default_path : unit -> unit;;

exception SWENotLoaded;;

