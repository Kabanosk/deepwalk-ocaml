open Graph_builder
open Model

module DeepWalk = struct
  let generate_walks graph node depth k =
    let rec gen_walks acc k' =
      if k' <= 0 then
        acc
      else
        let walk = GraphBuilder.random_walk graph node depth in
        gen_walks (walk :: acc) (k'-1)
    in
    gen_walks [] k

  let gen_dataset graph nodes depth k =
    let all_playlists =
      List.map fst nodes in
    let all_songs =
      List.sort_uniq compare (List.concat (List.map snd nodes)) in

    List.fold_left (fun acc node ->
      acc @ (generate_walks graph node depth k)
    ) [] (all_playlists @ all_songs)

  let data_preparation nodes walks =
    let create_mapping data =
      let unique_values = List.sort_uniq compare data in
      let index_map = Hashtbl.create (List.length unique_values) in
      List.iteri (fun i v -> Hashtbl.add index_map v i) unique_values;
      index_map
    in

    let indices_walks index_map walks =
      List.map (fun walk ->
        List.map (Hashtbl.find index_map) walk
      ) walks
    in

    let all_playlists = List.map fst nodes in
    let all_songs = List.concat (List.map snd nodes) in
    let index_map = create_mapping (all_playlists @ all_songs @ ["-1";]) in
    let vocabulary_size = Hashtbl.length index_map in
    let list_data = indices_walks index_map walks in
    let array_data = Array.of_list (List.map Array.of_list list_data) in
    (array_data, vocabulary_size)

  let run_deepwalk_pipeline ?(model_path="") graph nodes depth k =
    let dataset = gen_dataset graph nodes depth k in
    let prepared_data, vocab_size = data_preparation nodes dataset in
    let embedding_dim = 100 in
    let word2vec_model =
      if model_path = "" then
        Word2Vec.train_word2vec_skipgram
          vocab_size
          embedding_dim
          prepared_data
      else
        Word2Vec.load_model model_path
    in
    word2vec_model
end
