
(* parmap-like wrapper using parany *)
let parmap ~ncores ~csize ~f l =
  let input = ref l in
  let demux () = match !input with
    | [] -> raise Parany.End_of_input
    | x :: xs -> (input := xs; x) in
  let output = ref [] in
  let mux x =
    output := x :: !output in
  (* for safety *)
  Parany.set_copy_on_work ();
  Parany.set_copy_on_mux ();
  (* parallel work *)
  Parany.run ~verbose:false ~csize ~nprocs:ncores ~demux ~work:f ~mux;
  !output
