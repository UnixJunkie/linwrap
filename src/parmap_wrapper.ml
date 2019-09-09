
(* parmap-like wrapper using parany *)
let parmap ~ncores ~csize ~f l =
  let input = ref l in
  let demux () =
    match !input with
    | [] -> raise Parany.End_of_input
    | x :: xs ->
      let res = x in
      input := xs;
      res in
  let output = ref [] in
  let mux x =
    output := x :: !output
  in
  Parany.run ~verbose:false ~csize ~nprocs:ncores ~demux ~work:f ~mux;
  !output
