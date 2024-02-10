open Graph
open Spotify

module GraphBuilder = struct
  include Persistent.Graph.Concrete(struct
    type t = string
    let compare = String.compare
    let hash = Hashtbl.hash
    let equal = String.equal
  end)

  let g = empty
  let add_connection g v1 v2 = 
    let g_ = add_edge g v1 v2 in
    add_edge g_ v2 v1

  let build_graph filename = 
    let nodes = DE.get_nodes filename in  
    let all_playlists = List.map fst nodes in
    let all_songs = List.sort_uniq String.compare (List.concat (List.map snd nodes)) in
    
    let g = List.fold_left (fun g p -> add_vertex g p) g all_playlists in
    let g = List.fold_left (fun g s -> add_vertex g s) g all_songs in
  
    let rec build_edges graph nodes =
      match nodes with
      | [] -> graph
      | (p_id, songs) :: rest ->
          let g_ = List.fold_left (fun g s -> add_connection g p_id s) graph songs in
          build_edges g_ rest in
    build_edges g nodes

    let random_walk graph v d =
      let rec walk_rec acc node depth =
        if depth = 0 then
          List.rev acc
        else
          let nb = succ graph node in
          match nb with
          | [] -> (List.rev acc) @ (List.init depth (fun _ -> "-1"))
          | _ ->
            let random_neighbor = List.nth nb (Random.int (List.length nb)) in
            walk_rec (random_neighbor :: acc) random_neighbor (depth - 1)
      in
      walk_rec [v] v d
    
end
