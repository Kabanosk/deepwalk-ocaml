open Owl
open Neural.S
open Neural.S.Graph

module Word2Vec = struct
  let word2vec_skipgram vocab_size embedding_dim =
    input [|vocab_size|]
    |> embedding vocab_size embedding_dim
    |> lstm 128
    |> linear 512 ~act_typ:Activation.Relu
    |> linear vocab_size ~act_typ:Activation.(Softmax 1)
    |> get_network
  
  let params = Params.config
    ~batch:(Batch.Sample 100)
    ~verbosity:(true)
    ~learning_rate:(Learning_Rate.Adagrad 0.005) 
    1.
  
  let one_hot walk vocab_size =
    let vec = Array.make vocab_size 0. in
    Array.iter (fun x -> vec.(x) <- 1.) walk;
    vec
  
  let train_word2vec_skipgram vocab_size embedding_dim train_data =
    let network = word2vec_skipgram vocab_size embedding_dim in
    let x = 
      Neuron.Optimise.Algodiff.A.of_arrays (Array.map (fun arr -> one_hot arr vocab_size) train_data) 
    in
    let _ = Graph.train ~params network x x in
    network

  let get_word_embedding walk vocab_size trained_network =
    let input_data = Neuron.Optimise.Algodiff.A.of_array (one_hot walk vocab_size) [|vocab_size|] in
    let _, all_results = Graph.forward trained_network (Neuron.Optimise.Algodiff.pack_arr input_data) in
    all_results.(1)

  let save_model model filename =
    Owl_io.marshal_to_file model filename
  
  let load_model filename =
    Owl_io.marshal_from_file filename
end
