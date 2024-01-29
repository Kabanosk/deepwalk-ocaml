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

  include Persistent.Digraph.ConcreteBidirectional(struct
    type t = node
    let compare = compare_node
    let hash = Hashtbl.hash
    let equal = equal_node
  end)

  let g = empty
end

let build_graph filename = 
  let nodes = DE.get_nodes filename in
  let g = GraphBuilder.g in

  let all_playlists = List.map fst nodes in
  let all_songs = List.sort_uniq String.compare (List.concat (List.map snd nodes)) in
  
  let g = List.fold_left (fun g p -> GraphBuilder.add_vertex g (GraphBuilder.Playlist p)) g all_playlists in
  let g = List.fold_left (fun g s -> GraphBuilder.add_vertex g (GraphBuilder.Song s)) g all_songs in

  let rec build_edges graph nodes =
    match nodes with
    | [] -> graph
    | (p_id, songs) :: rest ->
        let g_ = List.fold_left (fun g s -> GraphBuilder.add_edge g (GraphBuilder.Playlist p_id) (GraphBuilder.Song s)) graph songs in
        build_edges g_ rest in
  
  build_edges g nodes
