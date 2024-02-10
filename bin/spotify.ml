open Yojson.Basic.Util

module DE = struct
  let get_songs filename =
    let json_str = Yojson.Basic.from_file filename in
    match json_str with
    | `Assoc _ ->
      let playlists = json_str |> member "playlists" |> to_list in
      List.fold_left (fun acc playlist ->
        let tracks = playlist |> member "tracks" |> to_list in
        let playlist_songs = List.map (fun track ->
          track |> member "track_uri" |> to_string
        ) tracks in
        acc @ playlist_songs
      ) [] playlists
    | _ -> failwith "Invalid JSON format" 
  
  let get_all_songs filenames =
    List.fold_left (fun acc filename ->
      acc @ get_songs filename
    ) [] filenames
  
  let get_playlists filename =
    let json_str = Yojson.Basic.from_file filename in 
    match json_str with
    | `Assoc _ ->
      json_str |> member "playlists" |> to_list
    | _ -> failwith "Invalid JSON format"
    
  let get_all_playlists filenames =
    List.fold_left (fun acc filename -> acc @ get_playlists filename) [] filenames

  let get_songs_from_playlist filename playlist =
    let json_str = Yojson.Basic.from_file filename in
    match json_str with
    | `Assoc _ ->
      let playlists = json_str |> member "playlists" |> to_list in
      (match 
        List.find_opt 
          (fun (pid, _) -> pid = playlist) 
          (List.fold_left 
            (fun acc pl -> (pl |> member "pid" |> to_int, pl |> member "tracks" |> to_list) :: acc)
            [] 
            playlists
          ) with
        | Some x -> snd x
        | None -> failwith "Playlist not found")
    | _ -> failwith "Invalid JSON format"
  
  let get_nodes filename = 
    let playlists = get_playlists filename in
    let json_nodes = List.fold_left
                      (fun acc pl -> (string_of_int (pl |> member "pid" |> to_int), pl |> member "tracks" |> to_list) :: acc)
                      []
                      playlists in
    List.map (fun node -> ((fst node), List.fold_left (fun acc song -> (song |> member "track_uri" |> to_string) :: acc) [] (snd node))) json_nodes
end