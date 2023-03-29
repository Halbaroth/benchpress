let pp_list =
  let pp_sep fmt () = Format.fprintf fmt " " in
  let pp_string fmt = Format.fprintf fmt "%s" in
  Format.pp_print_list ~pp_sep pp_string

let int_of_process_status = function
  | Unix.WEXITED i
  | Unix.WSIGNALED i
  | Unix.WSTOPPED i -> i

let () =
  let args = Sys.argv |> Array.to_list |> List.tl in
  if List.length args = 0 then failwith "Require a command";
  let cmd = Format.asprintf "%a" pp_list args in
  let stdout, stdin, stderr =
    Unix.open_process_full cmd (Unix.environment ())
  in
  let err = ref "" in
  let t_err = Thread.create (fun e -> err := In_channel.input_all e) stderr in
  let out = In_channel.input_all stdout in
  Thread.join t_err;
  let status = Unix.close_process_full (stdout, stdin, stderr) in
  let usage = Rusage.get Children in
  Format.fprintf (Format.std_formatter) "%f\n%f\n@?" usage.utime usage.stime;
  Format.fprintf (Format.std_formatter) "%s@?" out;
  Format.fprintf (Format.err_formatter) "%s@?" !err;
  exit (int_of_process_status status)
