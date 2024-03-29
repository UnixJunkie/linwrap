(* not using Batteries !!! Dont! I want fast IOs.
   Or we have to prefix many things with Legacy.XXX *)

(* Copyright (C) 2020, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

open Printf

module A = BatArray
module Fn = Filename
module L = BatList
module Log = Dolog.Log

type filename = string

let tap f x =
  f x;
  x

let fst3 (a, _, _) = a

let create_tmp_filename () =
  let res = Fn.temp_file
      ~temp_dir:"/tmp" "" (* no_prefix *) "" (* no_suffix *) in
  (* tap (Log.info "create_tmp_filename: %s") res; *)
  res

let mkfifo (fn: filename): unit =
  Unix.mkfifo fn 0o600

(* abort if condition is not met *)
let enforce (condition: bool) (err_msg: string): unit =
  if not condition then
    failwith err_msg

let with_out_file (fn: filename) (f: out_channel -> 'a): 'a =
  let output = open_out_bin fn in
  let res = f output in
  close_out output;
  res

let with_temp_out_file (f: filename -> 'a): 'a =
  let temp_file =
    Fn.temp_file ~temp_dir:"/tmp" "" (* no_prefix *) "" (* no_suffix *) in
  let res = f temp_file in
  Sys.remove temp_file;
  res

let with_in_file (fn: filename) (f: in_channel -> 'a): 'a =
  let input = open_in_bin fn in
  let res = f input in
  close_in input;
  res

let with_in_file2 fn1 fn2 f =
  let in1 = open_in_bin fn1 in
  let in2 = open_in_bin fn2 in
  let res = f in1 in2 in
  close_in in1;
  close_in in2;
  res

let with_in_file3 fn1 fn2 fn3 f =
  let in1 = open_in_bin fn1 in
  let in2 = open_in_bin fn2 in
  let in3 = open_in_bin fn3 in
  let res = f in1 in2 in3 in
  close_in in1;
  close_in in2;
  close_in in3;
  res

let with_infile_outfile (in_fn: filename) (out_fn: filename)
    (f: in_channel -> out_channel -> 'a): 'a =
  let input = open_in_bin in_fn in
  let output = open_out_bin out_fn in
  let res = f input output in
  close_in input;
  close_out output;
  res

let lines_of_file (fn: filename): string list =
  with_in_file fn (fun input ->
      let res, exn = L.unfold_exc (fun () -> input_line input) in
      if exn <> End_of_file then
        raise exn
      else res
    )

(* all lines of given file, in reverse order *)
let rev_lines_of_file (fn: filename): string list =
  with_in_file fn (fun input ->
      let res = ref [] in
      try
        while true do
          res := (input_line input) :: !res
        done;
        assert(false) (* never reached *)
      with End_of_file -> !res
    )

(* map f on lines of file *)
let map_on_lines_of_file (fn: filename) (f: string -> 'a): 'a list =
  L.rev_map f (rev_lines_of_file fn)

let lines_to_file fn l =
  with_out_file fn (fun out ->
      List.iter (fprintf out "%s\n") l
    )

let list_to_file (fn: filename) (f: 'a -> string) (l: 'a list): unit =
  lines_to_file fn (L.map f l)

let array_to_file (fn: filename) (f: 'a -> string) (a: 'a array): unit =
  with_out_file fn (fun out ->
      A.iter (fun x ->
          fprintf out "%s\n" (f x)
        ) a
    )

(* get one bootstrap sample of size 'nb_samples' using
   sampling with replacement *)
let array_bootstrap_sample rng nb_samples a =
  let n = Array.length a in
  assert(nb_samples <= n);
  A.init nb_samples (fun _ ->
      A.unsafe_get a (Random.State.int rng n)
    )

(* all lines of file [fn], except those starting with [comment_prefix] *)
let uncommented_lines_of_file
    (comment_prefix: string) (fn: filename): string list =
  let lines_rev = ref [] in
  with_in_file fn (fun input ->
      try
        while true do
          lines_rev := (input_line input) :: !lines_rev
        done
      with End_of_file -> ()
    );
  L.fold_left (fun acc l ->
      if BatString.starts_with l comment_prefix then
        acc
      else
        l :: acc
    ) [] !lines_rev

(* if the first line is a comment (starts with '#'); then we extract
   it separately from other lines; else we treat it as any other line *)
let maybe_extract_comment_header (fn: filename): string option * string list =
  let all_lines = lines_of_file fn in
  match all_lines with
  | [] -> (None, [])
  | fst :: others ->
    if BatString.starts_with fst "#" then
      (Some fst, others)
    else
      (None, all_lines)

(* call f on lines of file *)
let iter_on_lines_of_file fn f =
  let input = open_in_bin fn in
  try
    while true do
      f (input_line input)
    done
  with End_of_file -> close_in input

let iteri_on_lines_of_file fn f =
  let i = ref 0 in
  let input = open_in_bin fn in
  try
    while true do
      f !i (input_line input);
      incr i
    done
  with End_of_file -> close_in input

let map_on_file (fn: filename) (f: in_channel -> 'a): 'a list =
  with_in_file fn (fun input ->
      let res, exn = L.unfold_exc (fun () -> f input) in
      if exn = End_of_file then res
      else raise exn
    )

(* skip 'nb' blocks from file being read *)
let skip_blocks nb read_one input =
  if nb = 0 then ()
  else
    let () = assert(nb > 0) in
    for _ = 1 to nb do
      ignore(read_one input)
    done

let read_lines = lines_of_file

let write_lines (lines: string list) (output_fn: filename): unit =
  with_out_file output_fn (fun out ->
      List.iter (fprintf out "%s\n") lines
    )

let output_lines = write_lines

(* keep only lines that satisfy p *)
let filter_lines_of_file fn p =
  L.filter p (lines_of_file fn)

(* get the first line (stripped) output by given command *)
let get_command_output (cmd: string): string =
  Log.info "get_command_output: %s" cmd;
  let _stat, output = BatUnix.run_and_read cmd in
  match BatString.split_on_char '\n' output with
  | first_line :: _others -> first_line
  | [] -> (Log.fatal "get_command_output: no output for: %s" cmd; exit 1)

(* run the given command in a sub process (in parallel to the current process)
   and returns its pid so that we can wait for it later *)
let fork_out_cmd (cmd: string): int =
  Log.info "fork_out_cmd: %s" cmd;
  match Unix.fork () with
  | 0 -> (* child process *) exit (Sys.command cmd)
  | -1 -> (* error *) (Log.fatal "fork_out_cmd: fork failed"; exit 1)
  | pid -> pid

(* return full path of command, if found in PATH, none else *)
let command_exists (cmd: string): string option =
  let where_is_cmd = "which " ^ cmd in
  if Unix.system (where_is_cmd ^ " 2>&1 > /dev/null") = Unix.WEXITED 0 then
    Some (get_command_output where_is_cmd)
  else
    None

let run_command ?(debug = false) (cmd: string): unit =
  if debug then Log.info "run_command: %s" cmd;
  match Unix.system cmd with
  | Unix.WSIGNALED _ -> (Log.fatal "run_command: signaled: %s" cmd; exit 1)
  | Unix.WSTOPPED _ -> (Log.fatal "run_command: stopped: %s" cmd; exit 1)
  | Unix.WEXITED i when i <> 0 ->
    (Log.fatal "run_command: exit %d: %s" i cmd; exit 1)
  | Unix.WEXITED _ (* i = 0 then *) -> ()

let get_env (env_var: string): string option =
  try Some (Sys.getenv env_var)
  with Not_found -> None

(* look for exe in PATH then given env. var *)
let find_command (exe: string) (env_var: string): string option =
  match command_exists exe with
  | Some cmd -> Some cmd
  | None ->
    match get_env env_var with
    | Some cmd -> Some cmd
    | None -> (Log.warn "%s not found in PATH; \
                         put it in your PATH or setup the \
                         %s env. var. to point to it" exe env_var;
               None)

let filename_is_absolute fn =
  not (Fn.is_relative fn)

let relative_to_absolute fn =
  if Fn.is_relative fn then
    let cwd = Sys.getcwd () in
    Fn.concat cwd fn
  else
    fn

(* remove the prefix if it is there, or do nothing if it is not *)
let remove_string_prefix prfx str =
  if BatString.starts_with str prfx then
    let prfx_len = String.length prfx in
    BatString.tail str prfx_len
  else
    str

let string_contains super sub =
  try let _i = BatString.find super sub in true
  with Not_found -> false

let os_is_Mac_OS () =
  string_contains (get_command_output "uname -a") "Darwin"

let string_contains_non_binary_digit = Str.regexp "[^01]"

let string_contains_only_zeros_or_ones (s: string): bool =
  not (Str.string_match string_contains_non_binary_digit s 0)

let string_contains_non_digits_non_sep = Str.regexp "[^-0123456789;]"

let string_is_a_list_of_integers (s: string): bool =
  BatString.starts_with s "[" &&
  BatString.ends_with s "]" &&
  let chopped = BatString.chop ~l:1 ~r:1 s in
  not (Str.string_match string_contains_non_digits_non_sep chopped 0)

let may_apply f = function
  | Some x -> f x
  | None -> ()

(* returns true if we could create the file; false else (already there) *)
let lock_file_for_writing (fn: filename): bool =
  try
    let fd = Unix.(openfile fn [O_CREAT; O_EXCL; O_WRONLY] 0o600) in
    Unix.close fd;
    true
  with Unix.Unix_error _ -> false

(* compute all distinct pairs ((a,b) = (b,a)) of elements in 'l' *)
let all_pairs (l: 'a list): ('a * 'a) list =
  let pair x ys =
    L.map (fun y ->
        (x, y)
      ) ys
  in
  let rec loop acc = function
    | [] -> acc
    | x :: xs ->
      loop (L.rev_append (pair x xs) acc) xs
  in
  loop [] l

exception Enough_times

(* accumulate the result of calling 'f' 'n' times *)
let n_times n f =
  let i = ref 0 in
  BatList.unfold_exc (fun () ->
      if !i = n then raise Enough_times
      else
        let res = f () in
        incr i;
        res
    )

(* measure time spent in f (seconds) *)
let wall_clock_time f =
  let start = Unix.gettimeofday () in
  let res = f () in
  let stop = Unix.gettimeofday () in
  let delta_t = stop -. start in
  (delta_t, res)

let push (x: 'a) (l: 'a list ref): unit =
  l := x :: !l

(* the identity function *)
let id x = x

let one_or_more_spaces = Str.regexp "[ ]+"

let string_of_floats_array fv =
  let buff = Buffer.create 80 in
  Array.iter (fun f ->
      Buffer.add_string buff (sprintf "%f " f)
    ) fv;
  Buffer.contents buff

(* enforce filename uses one of the allowed extensions *)
let enforce_file_extension allowed_exts fn =
  assert(L.exists (BatString.ends_with fn) allowed_exts)

(* Pi math constant *)
let m_pi = 4.0 *. atan 1.0

let prepend x xs =
  xs := x :: !xs

(* test (lb <= x <= hb) *)
let in_bounds lb x hb =
  x >= lb && x <= hb

let list_medianf (l: float list): float =
  let xs = Array.of_list l in
  Array.sort BatFloat.compare xs;
  let n = Array.length xs in
  if n mod 2 = 1 then
    xs.(n/2)
  else
    0.5 *. (xs.(n/2) +. xs.(n/2 - 1))
(*$T list_medianf
   list_medianf [1.;2.;3.;4.;5.] = 3.0
   list_medianf [1.;2.;3.;4.] = 2.5
*)

let string_of_array ?pre:(pre = "[|") ?sep:(sep = ";") ?suf:(suf = "|]")
    to_str a =
  let buff = Buffer.create 80 in
  Buffer.add_string buff pre;
  Array.iteri (fun i x ->
      if i > 0 then Buffer.add_string buff sep;
      Buffer.add_string buff (to_str x)
    ) a;
  Buffer.add_string buff suf;
  Buffer.contents buff

(* marshal x to file *)
let save fn x =
  with_out_file fn (fun out ->
      Marshal.to_channel out x [Marshal.No_sharing]
    )

(* unmarshal x from file *)
let restore fn =
  with_in_file fn (fun input ->
      Marshal.from_channel input
    )

let marshal_to_string x =
  Marshal.(to_string x [No_sharing])

let unmarshal_from_string s =
  Marshal.from_string s 0

let is_odd i =
  i mod 2 = 1

let is_even i =
  i mod 2 = 0

let get_first_line fn =
  with_in_file fn input_line

(* like the cut unix command *)
let cut d f line =
  let splitted = BatString.split_on_char d line in
  BatList.at splitted f

let get_ncores () =
  int_of_string (get_command_output "getconf _NPROCESSORS_ONLN")

let int_of_bool = function
  | true -> 1
  | false -> 0

let bool_of_int = function
  | 1 -> true
  | 0 -> false
  | _ -> assert(false)

(* test that 'y' is within 'x' +/- 'epsilon';
   i.e. y \in [x - epsilon, x + epsilon] *)
let approx_equal epsilon x y =
  (y >= x -. epsilon) &&
  (y <= x +. epsilon)

(* proper NaN/nan testing *)
let is_nan x =
  match classify_float x with
  | FP_nan -> true
  | _ -> false

(* some statistics *)
let favg = function
  | [] -> 0.0 (* protect against empty list *)
  | xs -> L.favg xs

let fmin_max = function
  | [] -> (-. infinity, infinity) (* protect against empty list *)
  | xs -> L.min_max ~cmp:BatFloat.compare xs

(* population standard deviation *)
let stddev (l: float list): float =
  let n, sx, sx2 =
    List.fold_left (fun (n, sx, sx2) x ->
        (n +. 1., sx +. x, sx2 +. (x *.x))
      ) (0., 0., 0.) l
  in
  sqrt ((sx2 -. (sx *. sx) /. n) /. n)
(* stddev [2.; 4.; 4.; 4.; 5.; 5.; 7.; 9.] = 2.0 *)

let fincr_by (xref: float ref) (dx: float): unit =
  xref := !xref +. dx

let min_max x y =
  if x <= y then
    (x, y)
  else
    (y, x)

(* <=> wc -l fn *)
let file_nb_lines fn =
  let count = ref 0 in
  iter_on_lines_of_file fn (fun _line -> incr count);
  !count

let list_filter_count p l =
  let res = ref 0 in
  L.iter (fun x -> if p x then incr res) l;
  !res

let array_count p a =
  let i = ref 0 in
  A.iter (fun x -> if p x then incr i) a;
  !i

let list_combine3 l1 l2 l3 =
  let l1_l2 = L.combine l1 l2 in
  L.map2 (fun (x, y) z -> (x, y, z)) l1_l2 l3
