open Deep_walk
open Graph_builder
open Spotify
open Model

let print_graph graph =
  let nodes = DE.get_nodes "data/mpd.slice.0-999.json" in
  DeepWalk.run_deepwalk_pipeline 
    graph
    nodes
    10
    1

let graph = GraphBuilder.build_graph "data/mpd.slice.0-999.json"
let l = print_graph graph 
let create_mapping data =
  let unique_values = List.sort_uniq compare data in
  let index_map = Hashtbl.create (List.length unique_values) in
  List.iteri (fun i v -> Hashtbl.add index_map v i) unique_values;
  index_map    

open Owl
open Neural.S
open Neural.S.Graph

let print_algodiff_tensors matrix =
  let x = (Neuron.Optimise.Algodiff.unpack_arr matrix.(0)) in
  let num_elements = Algodiff.A.numel x in
  let flat_array = Array.make num_elements 0.0 in
  for i = 0 to num_elements - 1 do
    flat_array.(i) <- Algodiff.A.get x [|i|]
  done;
  Arr.print (Arr.of_array flat_array [|num_elements|])

let p model g = 
  let walk = GraphBuilder.random_walk g "0" 10 in
  let nodes = DE.get_nodes "data/mpd.slice.0-999.json" in
  let all_playlists = List.map fst nodes in
  let all_songs = List.concat (List.map snd nodes) in
  let index_map = create_mapping (all_playlists @ all_songs @ ["-1";]) in
  let vocab_size = Hashtbl.length index_map in
  let indices_walk index_map walk =
    List.map (Hashtbl.find index_map) walk    
  in
  
  let word_embedding = Word2Vec.get_word_embedding (Array.of_list (indices_walk index_map walk)) vocab_size model in
  print_algodiff_tensors word_embedding

  let () = p l graph
