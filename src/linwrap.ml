(* Copyright (C) 2019, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan.

   CLI wrapper on top of liblinear-tools.
   To train and test models using liblinear-train/predict *)

open Printf

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

let pred_score_of_pred_line l =
  match BatString.split_on_char ' ' l with
  | [pred_label; pred_score; _ignored] ->
    assert(pred_label = "-1" || pred_label = "1");
    float_of_string pred_score
  | _ -> assert(false)

module ROC = Cpm.MakeROC.Make(SL)

let main () =
  Log.(set_log_level INFO);
  Log.color_on ();
  let argc, args = CLI.init () in
  if argc = 1 then
    (eprintf "usage: %s\n  \
              -i <filename>: training set\n"
       Sys.argv.(0);
     exit 1);
  let input_fn = CLI.get_string ["-i"] args in
  (* let _output_fn = CLI.get_string ["-o"] args in *)
  let train_p = CLI.get_float_def ["-p"] args 0.8 in
  let rng = match CLI.get_int_opt ["--seed"] args with
    | None -> BatRandom.State.make_self_init ()
    | Some seed -> BatRandom.State.make [|seed|] in
  CLI.finalize ();
  let all_lines =
    (* randomize lines *)
    L.shuffle ~state:rng
      (Utls.lines_of_file input_fn) in
  let nb_lines = L.length all_lines in
  (* partition *)
  let train_card = BatFloat.round_to_int (train_p *. (float nb_lines)) in
  let train, test = L.takedrop train_card all_lines in
  (* train *)
  let train_fn = Filename.temp_file "linwrap_train_" ".txt" in
  let replaced, model_fn =
    (* liblinear places the model in the current working dir... *)
    BatString.replace ~str:(train_fn ^ ".model") ~sub:"/tmp/" ~by:"" in
  assert(replaced);
  Utls.lines_to_file train_fn train;
  Utls.run_command ~debug:true ("liblinear-train -s 0 " ^ train_fn);
  (* test *)
  let test_fn = Filename.temp_file "linwrap_test_" ".txt" in
  Utls.lines_to_file test_fn test;
  let preds_fn = Filename.temp_file "linwrap_preds_" ".txt" in
  (* compute AUC on test set *)
  Utls.run_command ~debug:true (sprintf "liblinear-predict -b 1 %s %s %s"
                                  test_fn model_fn preds_fn);
  (* extract true labels *)
  let true_labels = L.map (fun s -> BatString.starts_with s "+1 ") test in
  (* extact predicted scores *)
  let pred_lines = Utls.lines_of_file preds_fn in
  match pred_lines with
  | header :: preds ->
    begin
      assert(header = "labels 1 -1");
      let pred_scores = L.map pred_score_of_pred_line preds in
      let score_labels = L.map SL.create (L.combine true_labels pred_scores) in
      let auc = ROC.auc score_labels in
      Log.info "tstAUC: %.3f" auc
    end
  | _ -> assert(false)

let () = main ()
