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
module Log = Dolog.Log
module Opt = BatOption
module PHT = Dokeysto_camltc.Db_camltc.RW

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
  (* try *)
  Scanf.sscanf l "%d %f %f"
    (fun _pred_label pred_act_p _pred_dec_p ->
       pred_act_p
    )
(* with exn ->
   *   let () = Log.fatal "Linwrap.pred_score_of_pred_line: cannot parse: %s" l in
   *   raise exn *)

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

(* the sparse data file format for liblinear starts with the target_float_val
 * as the first field followed by idx:val space-separated FP values  *)
let get_pIC50 s =
  let pIC50_str, _fp = BatString.split s ~by:" " in
  (float_of_string pIC50_str)

let balanced_bag rng lines =
  let acts, decs = L.partition is_active lines in
  let n =
    let n_acts = L.length acts in
    let n_decs = L.length decs in
    min n_acts n_decs in
  let acts_a = array_bootstrap_sample rng n (A.of_list acts) in
  let decs_a = array_bootstrap_sample rng n (A.of_list decs) in
  let tmp_a = A.concat [acts_a; decs_a] in
  A.shuffle ~state:rng tmp_a; (* randomize selected lines order *)
  A.to_list tmp_a

(* what to do with the created models *)
type model_command = Restore_from of Utls.filename
                   | Save_into of Utls.filename
                   | Discard

let single_train_test verbose cmd c w train test =
  let quiet_command =
    if verbose then ""
    else "2>&1 > /dev/null" in
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
  begin match cmd with
    | Restore_from _ -> assert(false) (* not dealt with here *)
    | Discard -> L.iter (Sys.remove) [train_fn; test_fn; preds_fn; model_fn]
    | Save_into models_fn ->
      begin
        Utls.run_command (sprintf "echo %s >> %s" model_fn models_fn);
        L.iter (Sys.remove) [train_fn; test_fn; preds_fn]
      end
  end;
  match pred_lines with
  | header :: preds ->
    begin
      assert(header = "labels 1 -1");
      let pred_scores = L.map pred_score_of_pred_line preds in
      L.map SL.create (L.combine true_labels pred_scores)
    end
  | _ -> assert(false)

let single_train_test_regr verbose cmd e c train test =
  let quiet_option = if not verbose then "-q" else "" in
  (* train *)
  let train_fn = Filename.temp_file "linwrap_train_" ".txt" in
  Utls.lines_to_file train_fn train;
  let replaced, model_fn =
    (* liblinear places the model in the current working dir... *)
    BatString.replace ~str:(train_fn ^ ".model") ~sub:"/tmp/" ~by:"" in
  assert(replaced);
  Utls.run_command ~debug:verbose
    (sprintf "liblinear-train %s -s 11 -c %f -p %f %s %s"
       quiet_option c e train_fn model_fn);
  (* test *)
  let test_fn = Filename.temp_file "linwrap_test_" ".txt" in
  Utls.lines_to_file test_fn test;
  let preds_fn = Filename.temp_file "linwrap_preds_" ".txt" in
  (* compute R2 on test set *)
  Utls.run_command ~debug:verbose
    (sprintf "liblinear-predict %s %s %s %s"
       quiet_option test_fn model_fn preds_fn);
  let actual_values = L.map get_pIC50 test in
  let pred_lines = Utls.lines_of_file preds_fn in
  let nb_preds = L.length pred_lines in
  let test_card = L.length test in
  Utls.enforce (nb_preds = test_card)
    (sprintf "Linwrap.single_train_test_regr: |preds|=%d <> |test|=%d"
       nb_preds test_card);
  begin match cmd with
    | Restore_from _ -> assert(false) (* not dealt with here *)
    | Discard -> L.iter (Sys.remove) [train_fn; test_fn; preds_fn; model_fn]
    | Save_into models_fn ->
      begin
        Utls.run_command (sprintf "echo %s >> %s" model_fn models_fn);
        L.iter (Sys.remove) [train_fn; test_fn; preds_fn]
      end
  end;
  let pred_values = L.map float_of_string pred_lines in
  (actual_values, pred_values)

let accumulate_scores x y = match (x, y) with
  | ([], sl2) -> sl2
  | (sl1, []) -> sl1
  | (sl1, sl2) ->
    L.map2 (fun (l1, s1) (l2, s2) ->
        assert(l1 = l2);
        (l1, s1 +. s2)
      ) sl1 sl2

let average_scores k sls =
  assert(L.length sls = k);
  let sum = L.fold_left accumulate_scores [] sls in
  L.map (fun (l, s) -> (l, s /. (float k))) sum

let prod_predict ncores verbose model_fns test_fn output_fn =
  let quiet_command =
    if verbose then ""
    else "2>&1 > /dev/null" in
  let pred_fns =
    Parany.Parmap.parfold ncores
      (fun model_fn ->
         let preds_fn = Filename.temp_file "linwrap_preds_" ".txt" in
         Log.info "preds_fn: %s" preds_fn;
         Utls.run_command ~debug:verbose
           (* '-b 1' forces probabilist predictions instead of raw scores *)
           (sprintf "liblinear-predict -b 1 %s %s %s %s"
              test_fn model_fn preds_fn quiet_command);
         preds_fn)
      (fun acc preds_fn -> preds_fn :: acc)
      [] model_fns in
  (* all pred files should have this same number of predictions
     plus a header line *)
  let nb_rows = Utls.file_nb_lines test_fn in
  let card = 1 + nb_rows in
  Utls.enforce
    (L.for_all (fun fn -> card = (Utls.file_nb_lines fn)) pred_fns)
    "Linwrap.prod_predict: linwrap_preds_*.txt: different number of lines";
  let tmp_pht_fn = Filename.temp_file "linwrap_" ".pht" in
  let pht = PHT.create tmp_pht_fn in
  Log.info "Persistent hash table file: %s" tmp_pht_fn;
  let nb_models = L.length pred_fns in
  begin match pred_fns with
    | [] -> assert(false)
    | pred_fn_01 :: other_pred_fns ->
      begin
        (* populate ht *)
        Log.info "gathering %d models..." nb_models;
        Utls.iteri_on_lines_of_file pred_fn_01 (fun k line ->
            if k = 0 then
              assert(line = "labels 1 -1") (* check header *)
            else
              let pred_act_p = pred_score_of_pred_line line in
              let k_str = string_of_int k in
              PHT.add pht k_str (Utls.marshal_to_string pred_act_p)
          );
        (* accumulate *)
        L.iteri (fun i pred_fn ->
            Log.info "done: %d/%d" (i + 1) nb_models;
            Utls.iteri_on_lines_of_file pred_fn (fun k line ->
                if k = 0 then
                  assert(line = "labels 1 -1") (* check header *)
                else
                  let pred_act_p = pred_score_of_pred_line line in
                  let k_str = string_of_int k in
                  let prev_v: float =
                    Utls.unmarshal_from_string (PHT.find pht k_str) in
                  PHT.replace pht k_str
                    (Utls.marshal_to_string (pred_act_p +. prev_v))
              )
          ) other_pred_fns;
        Log.info "done: %d/%d" nb_models nb_models
      end
  end;
  (* write them to output file, averaged *)
  Utls.with_out_file output_fn (fun out ->
      for i = 1 to nb_rows do
        let k_str = string_of_int i in
        let sum_preds: float = Utls.unmarshal_from_string (PHT.find pht k_str) in
        fprintf out "%f\n" (sum_preds /. (float nb_models))
      done
    );
  PHT.close pht;
  (* PHT.destroy pht; *) (* FBR: UNCOMMENT AFTER DEBUG *)
  if verbose && output_fn <> "/dev/stdout" then
    (* compute AUC *)
    let auc =
      let true_labels = L.map is_active (Utls.lines_of_file test_fn) in
      let pred_scores =
        L.map (fun l -> Scanf.sscanf l "%f" (fun x -> x))
          (Utls.lines_of_file output_fn) in
      let score_labels = L.map SL.create (L.combine true_labels pred_scores) in
      ROC.auc score_labels in
    Log.info "AUC: %.3f" auc

let prod_predict_regr verbose model_fn test_fn output_fn =
  let quiet_option = if not verbose then "-q" else "" in
  Utls.run_command ~debug:verbose
    (sprintf "liblinear-predict %s %s %s %s"
       quiet_option test_fn model_fn output_fn)

let train_test ncores verbose cmd rng c w k train test =
  if k <= 1 then single_train_test verbose cmd c w train test
  else (* k > 1 *)
    let bags = L.init k (fun _ -> balanced_bag rng train) in
    let k_score_labels =
      Parany.Parmap.parmap ncores (fun bag ->
          single_train_test verbose cmd c w bag test
        ) bags in
    average_scores k k_score_labels

(* split a list into n parts (the last one might have less elements) *)
let list_nparts n l =
  let len = L.length l in
  assert(n <= len);
  let m = int_of_float (BatFloat.ceil ((float len) /. (float n))) in
  let rec loop acc = function
    | [] -> L.rev acc
    | lst ->
      let head, tail = L.takedrop m lst in
      loop (head :: acc) tail in
  loop [] l

(* create folds of cross validation; each fold consists in (train, test) *)
let cv_folds n l =
  let test_sets = list_nparts n l in
  assert(n = L.length test_sets);
  let rec loop acc prev curr =
    match curr with
    | [] -> acc
    | x :: xs ->
      let before_after = L.flatten (L.rev_append prev xs) in
      let prev' = x :: prev in
      let train_test = (before_after, x) in
      let acc' = train_test :: acc in
      loop acc' prev' xs in
  loop [] [] test_sets

let nfolds_train_test ncores verbose cmd rng c w k n dataset =
  assert(n > 1);
  L.flatten
    (L.map (fun (train, test) ->
         train_test ncores verbose cmd rng c w k train test
       ) (cv_folds n dataset))

let train_test_maybe_nfolds nfolds verbose model_cmd rng c' w' k' train test =
  let one_cpu = 1 in
  if nfolds <= 1 then
    train_test one_cpu verbose model_cmd rng c' w' k' train test
  else (* nfolds > 1 *)
    nfolds_train_test one_cpu verbose model_cmd rng c' w' k' nfolds
      (L.rev_append train test)

(* find the best threshold to do classification instead of ranking;
   by maximizing MCC over the threshold's range *)
let mcc_scan_proper ncores score_labels =
  let nsteps = 1001 in
  let thresholds = L.frange 0.0 `To 1.0 nsteps in
  let mccs =
    Parany.Parmap.parmap ncores (fun t ->
        let mcc = ROC.mcc t score_labels in
        (t, mcc)
      ) thresholds in
  let (_tmin, _mcc_min), (threshold, mcc_max) =
    L.min_max ~cmp:(fun (_t1, mcc1) (_t2, mcc2) ->
        BatFloat.compare (mcc1) (mcc2)
      ) mccs in
  (threshold, mcc_max)

let mcc_scan ncores verbose cmd rng c w k nfolds dataset =
  Utls.enforce (nfolds > 1) "Linwrap.mcc_scan: nfolds <= 1";
  let score_labels =
    nfolds_train_test ncores verbose cmd rng c w k nfolds dataset in
  let threshold, mcc_max = mcc_scan_proper ncores score_labels in
  Log.info "threshold: %f %dxCV_MCC: %f" threshold nfolds mcc_max

(* return the best parameter configuration found in the
   parameter configs list [cwks]:
   (best_c, best_w, best_k, best_auc) *)
let optimize ncores verbose nfolds model_cmd rng train test cwks =
  Parany.Parmap.parfold ncores
    (fun ((c', w'), k') ->
       let score_labels =
         train_test_maybe_nfolds
           nfolds verbose model_cmd rng c' w' k' train test in
       let auc = ROC.auc score_labels in
       (c', w', k', auc))
    (fun
      ((_c, _w, _k, prev_best_auc) as prev)
      ((c', w', k', curr_auc) as curr) ->
      if curr_auc > prev_best_auc then
        (Log.info "c: %f w1: %f k: %d AUC: %.3f" c' w' k' curr_auc;
         curr)
      else
        (Log.warn "c: %f w1: %f k: %d AUC: %.3f" c' w' k' curr_auc;
         prev)
    ) (-1.0, -1.0, -1, 0.5) cwks

(* find best (e, C) configuration by R2 maximization *)
let best_r2 l =
  L.fold_left (fun
                ((_best_e, _best_c, best_r2) as best)
                ((_curr_e, _curr_c, curr_r2) as new_best) ->
                if best_r2 >= curr_r2 then best else new_best
              ) (0.0, 0.0, 0.0) l

(* return the best parameter configuration (C, epsilon) found *)
let optimize_regr verbose ncores es cs train test =
  let e_c_r2s =
    Parany.Parmap.parmap ncores (fun e ->
        L.map (fun c ->
            let act, preds =
              single_train_test_regr verbose Discard e c train test in
            let r2 = Cpm.RegrStats.r2 act preds in
            (if verbose then
               (if r2 < 0.3 then
                  Log.error "(e, C, R2) = %f %f %.3f" e c r2
                else if r2 < 0.5 then
                  Log.warn "(e, C, R2) = %f %f %.3f" e c r2
                else
                  Log.info "(e, C, R2) = %f %f %.3f" e c r2
               )
            );
            (e, c, r2)
          ) cs
      ) es in
  best_r2 (L.map best_r2 e_c_r2s)

(* instance-wise normalization *)
let normalize_line l =
  let tokens = BatString.split_on_char ' ' l in
  match tokens with
  | [] -> failwith "Linwrap.normalize_line: empty line"
  | [_label] -> failwith ("Linwrap.normalize_line: no features: " ^ l)
  | label :: features ->
    let sum = ref 0 in
    let feat_vals =
      L.rev_map (fun feat_val_str ->
          Scanf.sscanf feat_val_str "%d:%d"
            (fun feat value ->
               sum := !sum + value;
               (feat, value))
        ) features in
    let feat_norm_vals =
      let total = float !sum in
      L.rev_map (fun (feat, value) ->
          (feat, (float value) /. total)
        ) feat_vals in
    let buff = Buffer.create 1024 in
    Buffer.add_string buff label;
    L.iter (fun (feat, norm_val) ->
        Printf.bprintf buff " %d:%f" feat norm_val
      ) feat_norm_vals;
    Buffer.contents buff

(* the uggliest unit test suite in the whole OCaml world *)
let () =
  assert(normalize_line "+1 2:1 5:8 123:1" =
         "+1 2:0.100000 5:0.800000 123:0.100000");
  assert(normalize_line "-1 2:3 4:7" = "-1 2:0.300000 4:0.700000")

let decode_w_range = function
  | None -> L.frange 1.0 `To 10.0 10 (* default w range *)
  | Some s ->
    try
      Scanf.sscanf s "%f:%d:%f" (fun start nsteps stop ->
          L.frange start `To stop nsteps
        )
    with exn ->
      begin
        Log.fatal "Linwrap.decode_w_range: invalid string: %s"  s;
        raise exn
      end

let decode_c_range (maybe_range_str: string option): float list =
  match maybe_range_str with
  | None -> (* default C range *)
    [0.01; 0.02; 0.05;
     0.1; 0.2; 0.5;
     1.; 2.; 5.;
     10.; 20.; 50.]
  | Some range_str ->
    L.map float_of_string
      (BatString.split_on_char ',' range_str)

(* (0 <= epsilon <= max_i(|y_i|)); according to:
   "Parameter Selection for Linear Support Vector Regression."
   Jui-Yang Hsia and Chih-Jen Lin.
   February 2020. IEEE Transactions on Neural Networks and Learning Systems.
   DOI: 10.1109/TNNLS.2020.2967637
   To optimize a SVR, we need to do the exponential scan of C
   for each epsilon value. *)
let svr_epsilon_range (nsteps: int) (ys: float list): float list =
  let maxi = L.max (L.rev_map (abs_float) ys) in
  Log.info "SVR epsilon range: [0:%f]; nsteps=%d" maxi nsteps;
  L.frange 0.0 `To maxi nsteps

let epsilon_range maybe_epsilon maybe_esteps train =
  match (maybe_epsilon, maybe_esteps) with
  | (Some _, Some _) -> failwith "Linwrap.epsilon_range: both e and esteps"
  | (None, None) -> failwith "Linwrap.epsilon_range: no e and no esteps"
  | (Some e, None) -> [e]
  | (None, Some nsteps) ->
    let train_pIC50s = L.map get_pIC50 train in
    (* FBR: might be nice to see: (min, avg+/-std, max) *)
    svr_epsilon_range nsteps train_pIC50s

(* FBR: support nfolds upon optimize_regr *)

let main () =
  Log.(set_log_level INFO);
  Log.color_on ();
  let argc, args = CLI.init () in
  if argc = 1 then
    (eprintf "usage: %s\n  \
              -i <filename>: training set or DB to screen\n  \
              [-o <filename>]: predictions output file\n  \
              [-np <int>]: ncores\n  \
              [-c <float>]: fix C\n  \
              [-e <float>]: fix epsilon (for SVR);\n  \
              (0 <= epsilon <= max_i(|y_i|))\n  \
              [-w <float>]: fix w1\n  \
              [-k <int>]: number of bags for bagging (default=off)\n  \
              [-n <int>]: folds of cross validation\n  \
              [--mcc-scan]: MCC scan for a trained model (requires n>1)\n  \
                            also requires (c, w, k) to be known\n  \
              [--seed <int>]: fix random seed\n  \
              [-p <float>]: training set portion (in [0.0:1.0])\n  \
              [--train <train.liblin>]: training set (overrides -p)\n  \
              [--valid <valid.liblin>]: validation set (overrides -p)\n  \
              [--test <test.liblin>]: test set (overrides -p)\n  \
              [{-l|--load} <filename>]: prod. mode; use trained models\n  \
              [{-s|--save} <filename>]: train. mode; save trained models\n  \
              [-f]: force overwriting existing model file\n  \
              [--scan-c]: scan for best C\n  \
              [--scan-e <int>]: epsilon scan #steps for SVR\n  \
              [--regr]: regression (SVR); also, implied by -e and --scan-e\n  \
              [--scan-w]: scan weight to counter class imbalance\n  \
              [--w-range <float>:<int>:<float>]: specific range for w\n  \
              (semantic=start:nsteps:stop)\n  \
              [--c-range <float,float,...>] explicit scan range for C \n  \
              (example='0.01,0.02,0.03')\n  \
              [--scan-k]: scan number of bags \
              (advice: optim. k rather than w)\n"
       Sys.argv.(0);
     exit 1);
  let input_fn = CLI.get_string_def ["-i"] args "/dev/null" in
  let maybe_train_fn = CLI.get_string_opt ["--train"] args in
  let maybe_valid_fn = CLI.get_string_opt ["--valid"] args in
  let maybe_test_fn = CLI.get_string_opt ["--test"] args in
  let output_fn = CLI.get_string_def ["-o"] args "/dev/stdout" in
  let will_save = L.mem "-s" args || L.mem "--save" args in
  let will_load = L.mem "-l" args || L.mem "--load" args in
  let force = CLI.get_set_bool ["-f"] args in
  Utls.enforce (not (will_save && will_load))
    ("Linwrap.main: cannot load and save at the same time");
  let model_cmd =
    begin match CLI.get_string_opt ["-s"; "--save"] args with
      | Some fn ->
        let () =
          Utls.enforce
            (force || not (Sys.file_exists fn))
            ("Linwrap: file already exists: " ^ fn) in
        Save_into fn
      | None ->
        begin match CLI.get_string_opt ["-l"; "--load"] args with
          | Some fn -> Restore_from fn
          | None -> Discard
        end
    end in
  let ncores = CLI.get_int_def ["-np"] args 1 in
  let train_p = CLI.get_float_def ["-p"] args 0.8 in
  assert(train_p >= 0.0 && train_p <= 1.0);
  let nfolds = CLI.get_int_def ["-n"] args 1 in
  let rng = match CLI.get_int_opt ["--seed"] args with
    | None -> BatRandom.State.make_self_init ()
    | Some seed -> BatRandom.State.make [|seed|] in
  let scan_C = CLI.get_set_bool ["--scan-c"] args in
  let fixed_c = CLI.get_float_opt ["-c"] args in
  let scan_w = CLI.get_set_bool ["--scan-w"] args in
  let w_range_str = CLI.get_string_opt ["--w-range"] args in
  let c_range_str = CLI.get_string_opt ["--c-range"] args in
  let k = CLI.get_int_def ["-k"] args 1 in
  let scan_k = CLI.get_set_bool ["--scan-k"] args in
  let do_mcc_scan = CLI.get_set_bool ["--mcc-scan"] args in
  let quiet = CLI.get_set_bool ["-q"] args in
  let fixed_w = CLI.get_float_opt ["-w"] args in
  let instance_wise_norm = CLI.get_set_bool ["--iwn"] args in
  Utls.enforce (not (L.mem "-e" args && L.mem "--scan-e" args))
    "Linwrap: -e and --scan-e are exclusive";
  let maybe_epsilon = CLI.get_float_opt ["-e"] args in
  let maybe_esteps = CLI.get_int_opt ["--scan-e"] args in
  let do_regression = Opt.is_some maybe_epsilon || Opt.is_some maybe_esteps in
  CLI.finalize (); (* ------------------------------------------------------ *)
  let verbose = not quiet in
  let lines_of_file fn =
    if instance_wise_norm then
      Utls.map_on_lines_of_file fn normalize_line
    else
      Utls.lines_of_file fn in
  (* scan C? *)
  let cs = match fixed_c with
    | Some c -> [c]
    | None ->
      if scan_C || BatOption.is_some c_range_str then
        decode_c_range c_range_str
      else [1.0] in
  (* scan w? *)
  let ws =
    if scan_w || BatOption.is_some w_range_str then
      decode_w_range w_range_str
    else match fixed_w with
      | Some w -> [w]
      | None -> [1.0] in
  (* scan k? *)
  let ks =
    if scan_k then [1; 2; 5; 10; 20; 50; 100]
    else [k] in
  let cwks = L.cartesian_product (L.cartesian_product cs ws) ks in
  match model_cmd with
  | Restore_from models_fn ->
    let model_fns = Utls.lines_of_file models_fn in
    prod_predict ncores verbose model_fns input_fn output_fn
  | Save_into (_)
  | Discard ->
    match maybe_train_fn, maybe_valid_fn, maybe_test_fn with
    | (None, None, None) ->
      begin
        let all_lines =
          (* randomize lines *)
          L.shuffle ~state:rng
            (lines_of_file input_fn) in
        if do_mcc_scan then
          begin match cs, ws, ks with
            | [c], [w], [k] ->
              (* we only try MCC scan for a model with known parameters *)
              mcc_scan ncores verbose model_cmd rng c w k nfolds all_lines
            | _, _, _ ->
              failwith "Linwrap: --mcc-scan: some hyper params are still free"
          end
        else
          let nb_lines = L.length all_lines in
          (* partition *)
          let train_card =
            BatFloat.round_to_int (train_p *. (float nb_lines)) in
          let train, test = L.takedrop train_card all_lines in
          if do_regression then
            let best_e, best_c, best_r2 =
              let epsilons = epsilon_range maybe_epsilon maybe_esteps train in
              optimize_regr verbose ncores epsilons cs train test in
            Log.info "best(e, C, R2) = %f %f %.3f"
              best_e best_c best_r2
          else (* classification *)
            let _best_c, _best_w, _best_k, _best_auc =
              optimize ncores verbose nfolds model_cmd rng train test cwks in
            ()
    end
    | (Some train_fn, Some valid_fn, Some test_fn) ->
      begin
        let train = lines_of_file train_fn in
        let best_c, best_w, best_k, best_valid_AUC =
          let valid = lines_of_file valid_fn in
          optimize ncores verbose nfolds model_cmd rng train valid cwks in
        Log.info "best (c, w, k) config: %f %f %d" best_c best_w best_k;
        Log.info "valAUC: %.3f" best_valid_AUC;
        let test_AUC =
          let test = lines_of_file test_fn in
          let score_labels =
            let one_cpu = 1 in
            train_test one_cpu verbose model_cmd rng best_c best_w best_k
              train test in
          ROC.auc score_labels in
        Log.info "tesAUC: %.3f" test_AUC
      end
    | _ -> failwith
             "Linwrap: --train, --valid and --test: provide all three or none"

let () = main ()
