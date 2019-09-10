(* Copyright (C) 2019, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan.

   CLI wrapper on top of liblinear-tools.
   To train and test models using liblinear-train/predict *)

open Printf

module A = BatArray
module CLI = Minicli.CLI
module L = BatList

module SL = struct
  type t = bool * float (* (label, pred_score) *)
  let create (l, s) =
    (l, s)
  let get_score (_l, s) =
    s
  let get_label (l, _s) =
    l
end

module ROC = Cpm.MakeROC.Make(SL)

let pred_score_of_pred_line l =
  match BatString.split_on_char ' ' l with
  | [pred_label; pred_score; _ignored] ->
    assert(pred_label = "-1" || pred_label = "1");
    float_of_string pred_score
  | _ -> assert(false)

(* get one bootstrap sample of size 'nb_samples' using
   sampling with replacement *)
let array_bootstrap_sample rng nb_samples a =
  let n = Array.length a in
  assert(nb_samples <= n);
  A.init nb_samples (fun _ ->
      let rand = Random.State.int rng n in
      a.(rand)
    )

let is_active s =
  BatString.starts_with s "+1 "

let balanced_bag rng lines =
  let acts, decs = L.partition is_active lines in
  let n_acts = L.length acts in
  let acts_a = array_bootstrap_sample rng n_acts (A.of_list acts) in
  let decs_a = array_bootstrap_sample rng n_acts (A.of_list decs) in
  let tmp_a = A.concat [acts_a; decs_a] in
  A.shuffle ~state:rng tmp_a (* randomize selected lines order *)

let train_test verbose quiet_command c w train test =
  (* train *)
  let train_fn = Filename.temp_file "linwrap_train_" ".txt" in
  Utls.lines_to_file train_fn train;
  let replaced, model_fn =
    (* liblinear places the model in the current working dir... *)
    BatString.replace ~str:(train_fn ^ ".model") ~sub:"/tmp/" ~by:"" in
  assert(replaced);
  Utls.run_command ~debug:verbose
    (sprintf "liblinear-train -c %f -w1 %f -s 0 %s %s"
       c w train_fn quiet_command);
  (* test *)
  let test_fn = Filename.temp_file "linwrap_test_" ".txt" in
  Utls.lines_to_file test_fn test;
  let preds_fn = Filename.temp_file "linwrap_preds_" ".txt" in
  (* compute AUC on test set *)
  Utls.run_command ~debug:verbose
    (* '-b 1' forces probabilist predictions instead of raw scores *)
    (sprintf "liblinear-predict -b 1 %s %s %s %s"
       test_fn model_fn preds_fn quiet_command);
  (* extract true labels *)
  let true_labels = L.map is_active test in
  (* extact predicted scores *)
  let pred_lines = Utls.lines_of_file preds_fn in
  match pred_lines with
  | header :: preds ->
    begin
      assert(header = "labels 1 -1");
      let pred_scores = L.map pred_score_of_pred_line preds in
      let score_labels =
        L.map SL.create (L.combine true_labels pred_scores) in
      let auc = ROC.auc score_labels in
      Log.info "c: %.3f w1: %.1f tstAUC: %.3f" c w auc
    end
  | _ -> assert(false)

let main () =
  Log.(set_log_level INFO);
  Log.color_on ();
  let argc, args = CLI.init () in
  if argc = 1 then
    (eprintf "usage: %s\n  \
              -i <filename>: training set\n  \
              [-k <int>]: number of bags for bagging (default=off)\n  \
              [-np <int>]: ncores\n  \
              [-c <float>]: fix C\n  \
              [-w <float>]: fix w1\n  \
              [--scan-C]: scan for best C\n  \
              [--scan-W]: scan weight to counter class imbalance\n"
       Sys.argv.(0);
     exit 1);
  let input_fn = CLI.get_string ["-i"] args in
  let ncores = CLI.get_int_def ["-np"] args 1 in
  let _maybe_k = CLI.get_int_opt ["-k"] args in
  (* let _output_fn = CLI.get_string ["-o"] args in *)
  let train_p = CLI.get_float_def ["-p"] args 0.8 in
  let rng = match CLI.get_int_opt ["--seed"] args with
    | None -> BatRandom.State.make_self_init ()
    | Some seed -> BatRandom.State.make [|seed|] in
  let scan_C = CLI.get_set_bool ["--scan-C"] args in
  let fixed_c = CLI.get_float_opt ["-c"] args in
  let scan_w = CLI.get_set_bool ["--scan-W"] args in
  let quiet = CLI.get_set_bool ["-q"] args in
  let fixed_w = CLI.get_float_opt ["-w"] args in
  CLI.finalize ();
  let verbose = not quiet in
  let quiet_command =
    if verbose then ""
    else "2>&1 > /dev/null" in
  let all_lines =
    (* randomize lines *)
    L.shuffle ~state:rng
      (Utls.lines_of_file input_fn) in
  let nb_lines = L.length all_lines in
  (* partition *)
  let train_card = BatFloat.round_to_int (train_p *. (float nb_lines)) in
  let train, test = L.takedrop train_card all_lines in
  (* scan C *)
  let cs = match fixed_c with
    | Some c -> [c]
    | None ->
      if scan_C then
        [0.001; 0.002; 0.005;
         0.01; 0.02; 0.05;
         0.1; 0.2; 0.5;
         1.; 2.; 5.;
         10.; 20.; 50.; 100.]
      else [1.0] in
  let weights =
    if scan_w then L.frange 1.0 `To 50.0 50
    else match fixed_w with
      | Some w -> [w]
      | None -> [1.0] in
  Parmap_wrapper.pariter ~ncores ~csize:1 ~f:(fun c ->
      L.iter (fun w ->
          train_test verbose quiet_command c w train test
        ) weights
    ) cs

let () = main ()
