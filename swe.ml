open Ctypes
open Foreign

type planet =
  | Ecl_nut
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
  | Waldemath;;

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
  | Pisces;;

let zodiac_sign_string ?(short=false) zs =
  let s = match zs with
    | Aries -> "Aries"
    | Taurus -> "Taurus"
    | Gemini -> "Gemini"
    | Cancer -> "Cancer"
    | Leo -> "Leo"
    | Virgo -> "Virgo"
    | Libra -> "Libra"
    | Scorpio -> "Scorpio"
    | Sagittarius -> "Sagittarius"
    | Capricorn -> "Capricorn"
    | Aquarius -> "Aquarius" 
    | Pisces -> "Pisces"
  in if short then
    let r = (String.sub s 0 3)
    in r
    else s;;

type zodiac_sign_long= Zodiac of zodiac_sign * float;;

exception SignError of int;;

let sign_long_num i =
  match i with
    0 -> Aries
  | 1 -> Taurus
  | 2 -> Gemini
  | 3 -> Cancer
  | 4 -> Leo
  | 5 -> Virgo
  | 6 -> Libra
  | 7 -> Scorpio
  | 8 -> Sagittarius
  | 9 -> Capricorn
  | 10 -> Aquarius
  | 11 -> Pisces
  | _ -> raise (SignError i);;

let planet_id_num p =
  match p with
  | Ecl_nut -> -1
  | Sun -> 0
  | Moon -> 1
  | Mercury -> 2
  | Venus -> 3       
  | Mars -> 4       
  | Jupiter -> 5
  | Saturn -> 6
  | Uranus -> 7       
  | Neptune -> 8       
  | Pluto -> 9       
  | Mean_node -> 10      
  | True_node -> 11
  | Mean_apog -> 12      
  | Oscu_apog -> 13    
  | Earth -> 14      
  | Chiron -> 15      
  | Pholus -> 16      
  | Ceres  -> 17      
  | Pallas -> 18      
  | Juno   -> 19      
  | Vesta   -> 20      
  | Intp_apog -> 21      
  | Intp_perg  -> 22    

  | Cupido -> 40
  | Hades -> 41
  | Zeus -> 42
  | Kronos -> 43
  | Apollon -> 44
  | Admetos -> 45
  | Vulkanus -> 46
  | Poseidon -> 47
    
  | Isis -> 48
  | Nibiru -> 49
  | Harrington -> 50
  | Neptune_leverrier -> 51
  | Neptune_adams   ->   52
  | Pluto_lowell    ->   53
  | Pluto_pickering ->   54
  | Vulcan          ->   55
  | White_moon      ->   56
  | Proserpina      ->   57
  | Waldemath       ->   58

let planet_name p  =
    match p with
  | Ecl_nut -> "Ecl_nut"
  | Sun -> "Sun"
  | Moon -> "Moon"
  | Mercury -> "Mercury"
  | Venus -> "Venus"
  | Mars -> "Mars"
  | Jupiter -> "Jupiter"
  | Saturn -> "Saturn"
  | Uranus -> "Uranus"
  | Neptune -> "Neptune" 
  | Pluto -> "Pluto"
  | Mean_node -> "Mean_node"     
  | True_node -> "True_node"
  | Mean_apog -> "Mean_apog"
  | Oscu_apog -> "Oscu_apog"
  | Earth -> "Earth"
  | Chiron -> "Chiron"  
  | Pholus -> "Pholus"
  | Ceres  -> "Ceres"
  | Pallas -> "Pallas"
  | Juno   -> "Juno"  
  | Vesta   -> "Vesta"
  | Intp_apog -> "Intp_apog"
  | Intp_perg  -> "Intp_perg"

  | Cupido -> "Cupido"
  | Hades -> "Hades"
  | Zeus -> "Zeus"
  | Kronos -> "Kronus"
  | Apollon -> "Apollon"
  | Admetos -> "Admetos"
  | Vulkanus -> "Vulkanus"
  | Poseidon -> "Poseidon"
    
  | Isis -> "Isis"
  | Nibiru -> "Nibiru"
  | Harrington -> "Harrington"
  | Neptune_leverrier -> "Neptune_leverrier"
  | Neptune_adams   ->   "Neptune_adams"
  | Pluto_lowell    ->   "Pluto_lowell"
  | Pluto_pickering ->   "Pluto_pickering"
  | Vulcan          ->   "Vulcan"
  | White_moon      ->   "White_moon"
  | Proserpina      ->   "Proserpina"
  | Waldemath       ->   "Waldemath"
    
type sw_latlong =
  { sw_long : float;
    sw_lat : float;
    sw_dist : float;
    sw_speedlong : float;
    sw_speedlat : float;
    sw_speeddist : float;
  };;

let zodiac_long_pos long =
  let sign_n = (((int_of_float long) / 30))
  and sign_pos = (mod_float long 30.0)
  in Zodiac((sign_long_num sign_n), sign_pos)

let zodiac_str ?(short_sign=false) zp =
  match zp with
    Zodiac(sign,pos) -> Printf.sprintf "%.1f %s" pos (zodiac_sign_string ~short:short_sign sign);;

let zodiac_string ?(short_sign=false) swr =
  let zp = zodiac_long_pos swr.sw_long
  in zodiac_str zp;;



type coordinate_center = HELIO | GEO;;
type coordinate_system = POLAR | XYZ;;

let null_sw_result () =
  {
    sw_long = 0.0;
    sw_lat = 0.0;
    sw_dist = 0.0;
    sw_speedlong = 0.0;
    sw_speedlat = 0.0;
    sw_speeddist = 0.0;
  };;

let print_sw_header () =
  Printf.printf "Body\tSW_LONG\t\tSign long\tSW_LAT\tSW_DIST\tSW_SPEED\n";;

let print_sw_result_debug planet_name sw =
  let zodiac_pos = zodiac_long_pos sw.sw_long
  in 
  Printf.printf "%5s\t%.2f\t%12s\t%.2f\t%.2f\t%.3f\t%.3f\t%.3f\n" planet_name sw.sw_long (zodiac_str ~short_sign:true zodiac_pos) sw.sw_lat sw.sw_dist sw.sw_speedlong sw.sw_speedlat sw.sw_speeddist;;

exception SWENotLoaded;;

let swelib = ref None;;

let get_swelib () =
  match !swelib with
    None -> raise SWENotLoaded
  | (Some l) -> l;;

let load_library_path so_filename =
  swelib := Some (Dl.dlopen ~filename:so_filename ~flags:[Dl.RTLD_NOW]);;

let load_library_default_path () =
  load_library_path "/usr/local/lib64/libswe.so"

(* Direct SWE bindings *)
let _hel_date () =
  foreign ~from:(get_swelib ()) "w_swe_calc_ut" (int @-> int @-> int @->
                                        float @-> int @-> ptr double @-> long @-> returning long);;

let fr_swe_date_conversion () = foreign ~from:(get_swelib ())
    "swe_date_conversion" (int @-> int @-> int @-> double @-> char @-> ptr double @-> returning int);;

let fr_swe_calc_ut () = foreign ~from:(get_swelib ())
    "swe_calc_ut" (double @-> int @-> int @-> ptr double @-> ptr char @-> returning int);;

let fr_swe_revjul () = foreign ~from:(get_swelib ())
    "swe_revjul" (double @-> int @-> ptr int @-> ptr int @-> ptr int @-> ptr double @-> returning void);;

let fr_swe_utc_to_jd () = foreign ~from:(get_swelib ())
    "swe_utc_to_jd" (int32_t @-> int32_t @-> int32_t @-> int32_t @-> int32_t @-> double @-> int @-> ptr double @-> ptr char @-> returning int32_t);;

let fr_swe_fixstar_ut () = foreign ~from:(get_swelib ())
    "swe_fixstar_ut" (ptr char @-> double @-> long @-> ptr double @-> ptr char @-> returning long);;
                                      
let result_failed res = (res < (Signed.Long.of_int 0));;

exception SWEFailed
exception SWEFailedErr of string;;

let sw_err_buffer = CArray.make char 256;;

type cal = CAL_GREG | CAL_JUL;;

let create_request_flag center system =
  let flag1 = (match center with HELIO -> 8 | GEO ->  0)
  and flag2 = (match system with POLAR -> 0 | XYZ ->  4096)
  in (flag1 lor flag2 lor 256);;

let julian_day_of_date ?(hour=12) ?(min=0) ?(dsec=0.0) ?(cal=CAL_GREG) ~year ~month ~day =
  let dr = CArray.make double 2
  and gregflag = match cal with CAL_GREG -> 0 | _ -> 1
  in let res = (fr_swe_utc_to_jd ()) (Int32.of_int year) (Int32.of_int month) (Int32.of_int day) (Int32.of_int hour) (Int32.of_int min) dsec gregflag (dr.Ctypes_static.astart) (sw_err_buffer.Ctypes_static.astart)
  in if (res = (Int32.of_int 0)) then
    CArray.get dr 1
  else raise SWEFailed;;

let char_array_as_string a =
    let len = Ctypes.CArray.length a in 
    let b = Buffer.create len in 
    try 
      for i = 0 to len -1 do 
        let c = CArray.get a i in 
        if c = '\x00' then raise Exit else Buffer.add_char b c
      done;
      Buffer.contents b 
    with Exit -> Buffer.contents b;;

let planet_on_jd_ut jd_ut ?(flag_override=None) ?(center=GEO) ?(cal=CAL_JUL) ?(system=POLAR)
    ?(sw_results=CArray.make double 6) planet =
  let lflag =
    match flag_override with
    | None -> create_request_flag center system
    | (Some fo) -> fo
  in
  let res = (fr_swe_calc_ut ()) jd_ut (planet_id_num planet) lflag (sw_results.Ctypes_static.astart) (sw_err_buffer.Ctypes_static.astart)
  in if (res >= 0) then
    {
      sw_long = CArray.get sw_results 0;
      sw_lat = CArray.get sw_results 1;
      sw_dist = CArray.get sw_results 2;
      sw_speedlong = CArray.get sw_results 3;
      sw_speedlat = CArray.get sw_results 4;
      sw_speeddist = CArray.get sw_results 5;
    }
  else raise (SWEFailedErr (char_array_as_string sw_err_buffer));;

let planet_on_date ~year ~month ~day ?(hour=12) ?(min=0) ?(sec=0.0)
    ?(flag_override=None) ?(center=GEO) ?(cal=CAL_JUL) ?(system=POLAR) ?(sw_results=CArray.make double 6)
    planet =
  let lflag =
    match flag_override with
    | None -> create_request_flag center system
    | (Some fo) -> fo
  in
  let jd_ut = julian_day_of_date ~hour:hour ~min:min ~dsec:sec ~year:year ~month:month ~day:day ~cal:CAL_JUL
  in
  let res = (fr_swe_calc_ut ()) jd_ut (planet_id_num planet) lflag (sw_results.Ctypes_static.astart) (sw_err_buffer.Ctypes_static.astart)
  in if (res >= 0) then
    {
      sw_long = CArray.get sw_results 0;
      sw_lat = CArray.get sw_results 1;
      sw_dist = CArray.get sw_results 2;
      sw_speedlong = CArray.get sw_results 3;
      sw_speedlat = CArray.get sw_results 4;
      sw_speeddist = CArray.get sw_results 5;
    }
  else raise (SWEFailedErr (char_array_as_string sw_err_buffer));;

let std_calendar = CAL_JUL;;

let day_diff (y1,m1,d1) (y2,m2,d2) =
  let jd_ut1 = julian_day_of_date ~hour:0 ~min:0 ~dsec:0.0 ~year:y1 ~month:m1 ~day:d1 ~cal:std_calendar
  and jd_ut2 = julian_day_of_date ~hour:0 ~min:0 ~dsec:0.0 ~year:y2 ~month:m2 ~day:d2 ~cal:std_calendar
  in jd_ut2 -. jd_ut1;;

let day_to_jd (y,m,d) =
  let jd_ut = julian_day_of_date ~hour:0 ~min:0 ~dsec:0.0 ~year:y ~month:m ~day:d ~cal:std_calendar
  in jd_ut;;

let jd_to_day jd =
  let (y,m,d,h) = ((allocate int 0), (allocate int 0), (allocate int 0), (allocate double 0.0))
  in (fr_swe_revjul ()) jd 1 y m d h;
  (!@ y,!@ m, !@d);;

exception DateError of string

let find_matching_jds_planet f ~jd1 ~jd2
    ?(flag_override=None) ?(center=GEO) ?(cal=CAL_JUL) ?(system=POLAR) planet =
  let jd_min = min jd1 jd2
  and jd_max = max jd1 jd2
  in let sw_results_d = CArray.make double 6
  in 
  let rec scanner jd found =
    (*    Printf.printf "jd=%.1f\n" jd; *)
    if jd > jd_max then found
    else
      begin
        let sw_results = planet_on_jd_ut jd planet ~center:center ~cal:cal ~system:system ~sw_results:sw_results_d
        in
        if (f sw_results) then scanner (jd +. 1.0) (jd::found)
        else scanner (jd +. 1.0) found
      end
  in scanner jd_min [];;

let find_matching_jds_planets2 ?(planet2_offset_jd=0.0) f ~jd1 ~jd2
    ?(flag_override=None) ?(center=GEO) ?(cal=CAL_JUL) ?(system=POLAR) planet1 planet2 =
  let jd_min = min jd1 jd2
  and jd_max = max jd1 jd2
  in let sw_results_d1 = CArray.make double 6
  and sw_results_d2 = CArray.make double 6
  in 
  let rec scanner jd found =
    (*    Printf.printf "jd=%.1f\n" jd; *)
    if jd > jd_max then found
    else
      begin
        let sw_results1 = planet_on_jd_ut jd planet1 ~center:center ~cal:cal ~system:system ~sw_results:sw_results_d1
        and sw_results2 = planet_on_jd_ut (jd+.planet2_offset_jd) planet2 ~center:center ~cal:cal ~system:system ~sw_results:sw_results_d2
        in
        if (f sw_results1 sw_results2) then scanner (jd +. 0.25) (jd::found)
        else scanner (jd +. 0.25) found
      end
  in scanner jd_min [];;

let helio_planet_on_date = planet_on_date ~center:HELIO;;
let geo_planet_on_date = planet_on_date ~center:GEO;;



