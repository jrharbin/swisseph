open Swe
open Render
open ANSITerminal
open Core
open Core.Std

let table_format = create_table_fixed_width ([
    { name = "Planet"; styles = [Background(Black); Foreground(Yellow)]; printer = (fun (p,swr) -> Swe.planet_name p); fixed_width = (Some 15) };
    { name = "Longitude"; styles = [Background(Black); Foreground(White)]; printer = (fun (p,swr) -> Printf.sprintf "%.2f" swr.sw_long); fixed_width = None };
    { name = "Zodiac Sign";  styles = [Background(Black); Foreground(White)]; printer = (fun (p,swr) -> Printf.sprintf "%s" (Swe.zodiac_string swr)); fixed_width = (Some 20) };
    { name = "Dec.";   styles = [Background(Black); Foreground(Green)]; printer = (fun (p,swr) -> Printf.sprintf "%.1f" swr.sw_lat); fixed_width = None };
    { name = "Dist.";   styles = [Background(Black); Foreground(Red)]; printer = (fun (p,swr) -> Printf.sprintf "%.2f" swr.sw_dist); fixed_width = None };
    { name = "Speed long."; styles = [Background(Red); Foreground(White)]; printer = (fun (p,swr) -> Printf.sprintf "%.3f" swr.sw_speedlong); fixed_width = None };
    { name = "Speed lat.";  styles = [Background(Green); Foreground(White)]; printer = (fun (p,swr) -> Printf.sprintf "%.3f" swr.sw_speedlat); fixed_width = None };
    { name = "Speed dist."; styles = [Background(Blue); Foreground(White)]; printer = (fun (p,swr) -> Printf.sprintf "%.3f" swr.sw_speeddist); fixed_width = None };
]);;

let planets = [Moon; Sun; Mercury; Venus; Mars; Jupiter; Saturn; Uranus; Neptune; Pluto];;

let display_ephemeris_date (y,m,d) =
      let geo_res = List.map ~f:(fun planet -> (planet, (planet_on_date ~center:GEO ~year:y ~month:m ~day:d ~hour:12 planet))) planets
      and helio_res = List.map ~f:(fun planet -> (planet, (planet_on_date ~center:HELIO ~year:y ~month:m ~day:d ~hour:12 planet))) planets
      in
      ANSITerminal.printf [Background(White); Foreground(Black)] "                                     Results for %u-%u-%u GEOCENTRIC                                  \n" y m d;
      Render.print_table table_format geo_res;
      ANSITerminal.printf [Background(White); Foreground(Black)] "                                     Results for %u-%u-%u HELIOCENTRIC                                \n" y m d;
      Render.print_table table_format helio_res
