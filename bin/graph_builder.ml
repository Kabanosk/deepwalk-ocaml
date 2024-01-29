open Graph
open Spotify

module GraphBuilder = struct
  type node =
    | Playlist of int
    | Song of string
  
  let compare_node a b =
    match (a, b) with
    | Playlist x, Playlist y -> compare x y
    | Song x, Song y -> String.compare x y
    | Playlist _, Song _ -> -1
    | Song _, Playlist _ -> 1

  let equal_node a b =
    match (a, b) with
    | Playlist x, Playlist y -> x = y
    | Song x, Song y -> String.equal x y
    | _ -> false

  include Persistent.Graph.Concrete(struct
    type t = node
    let compare = compare_node
    let hash = Hashtbl.hash
    let equal = equal_node
  end)

  let g = empty
  let add_connection g v1 v2 = 
    let g_ = add_edge g v1 v2 in
    add_edge g_ v2 v1

  let build_graph filename = 
    let nodes = DE.get_nodes filename in  
    let all_playlists = List.map fst nodes in
    let all_songs = List.sort_uniq String.compare (List.concat (List.map snd nodes)) in
    
    let g = List.fold_left (fun g p -> add_vertex g (Playlist p)) g all_playlists in
    let g = List.fold_left (fun g s -> add_vertex g (Song s)) g all_songs in
  
    let rec build_edges graph nodes =
      match nodes with
      | [] -> graph
      | (p_id, songs) :: rest ->
          let g_ = List.fold_left (fun g s -> add_connection g (Playlist p_id) (Song s)) graph songs in
          build_edges g_ rest in
    
    build_edges g nodes

    let random_walk graph v d =
      let rec walk_rec acc node depth =
        if depth = 0 then
          acc
        else
          let nb = succ graph node in
          match nb with
          | [] -> acc
          | _ ->
            let random_neighbor = List.nth nb (Random.int (List.length nb)) in
            walk_rec (random_neighbor :: acc) random_neighbor (depth - 1)
      in
      walk_rec [v] v d
    
end
