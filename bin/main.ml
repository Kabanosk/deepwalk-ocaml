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
open Graph_builder

let pr v = 
  match v with
  | GraphBuilder.Song s -> Printf.printf "-> %s\n" s
  | GraphBuilder.Playlist p -> Printf.printf "-> %d\n" p

(* let pr2 v1 v2 = 
  match (v1, v2) with
  | GraphBuilder.Playlist p, GraphBuilder.Song s -> Printf.printf "Edge: %d -> %s\n" p s
  | GraphBuilder.Song s, GraphBuilder.Playlist p -> Printf.printf "h: %s -> %d\n" s p
  | _ -> () *)

(* let rw graph n d =
  let w = GraphBuilder.random_walk graph (GraphBuilder.Playlist 0) 5 in
  List.map (fun n ->
    match n with
    | GraphBuilder.Playlist p -> p
    | GraphBuilder.Song s -> s
    | _ -> failwith "Unexpected vertex type"
  ) w *)

let print_graph graph : unit =
  List.iter (fun v -> pr v) (GraphBuilder.random_walk graph (GraphBuilder.Playlist 450) 20)

let () = print_graph (GraphBuilder.build_graph "data/mpd.slice.0-999.json")
