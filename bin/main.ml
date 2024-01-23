(* let remove_duplicates lst =
  List.sort_uniq compare lst *)

(**
let () =
  let directory = "data/" in
  let num_of_files = 7 in
  let files = (
    List.map 
      (fun file -> (Filename.concat directory file)) 
      (Array.to_list (Array.sub (Sys.readdir directory) 0 num_of_files))
  ) in
  let songs = remove_duplicates (Spotify.DE.get_all_songs files) in
  let playlists = remove_duplicates (Spotify.DE.get_all_playlists files) in
  Printf.printf 
    "num songs: %d\nnum playlists: %d\n" 
      (List.length songs) 
      (List.length playlists)
**)

open Yojson.Basic.Util

let () = 
  let songs = Spotify.DE.get_nodes "data/mpd.slice.0-999.json" in
  List.iter (fun s ->
    Printf.printf "pid: %d\n" (fst s); 
    List.iter (fun x -> Printf.printf "\t%d: %s\n" (x |> member "pos" |> to_int) (x |> member "track_name" |> to_string)) (snd s)
  ) songs