open Graph_builder

module DeepWalk = struct
  let ganerate_walks graph node depth k = 
   let rec gen_walks acc k' =
    if k' <= 0 then
      acc
    else 
      let walk = GraphBuilder.random_walk graph node depth in
      gen_walks (walk :: acc) (k'-1)
    in
    gen_walks [] k

  (* let deep_walk graph start_vertex walk_length num_walks = () *)
end