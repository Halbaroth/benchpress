let pp_list =
  let pp_sep fmt () = Format.fprintf fmt " " in
  let pp_string fmt = Format.fprintf fmt "%s" in
  Format.pp_print_list ~pp_sep pp_string

let int_of_process_status = function
  | Unix.WEXITED i
  | Unix.WSIGNALED i
  | Unix.WSTOPPED i -> i

let main show_output prog args =
  let cmd = Format.asprintf "%s %a" prog pp_list args in
  let stdout, stdin, stderr =
    Unix.open_process_full cmd (Unix.environment ())
  in
  let err = ref "" in
  let t_err = Thread.create (fun e -> err := In_channel.input_all e) stderr in
  let out = In_channel.input_all stdout in
  Thread.join t_err;
  let status = Unix.close_process_full (stdout, stdin, stderr) in
  let usage = Rusage.get Children in
  Format.printf "%f\n%f\n@!" usage.utime usage.stime;
  if show_output then begin
    Format.fprintf (Format.std_formatter) "%s@!" out;
    Format.fprintf (Format.err_formatter) "%s@!" !err;
  end;
  exit (int_of_process_status status)

module Cmd = struct
  open Cmdliner

  let show_output =
    let doc = "Show the stdout and stderr of the program" in
    Arg.(
      value & flag & info ["s"; "show-output"] ~doc)

  let prog =
    Arg.(required & pos 0 (some string) None (Arg.info [] ~docv:"PROG"))

  and args = Arg.(value & pos_right 0 string [] (Arg.info [] ~docv:"ARGS"))

  let cmd =
    let doc = "Wrapper" in
    let man =
      [
        `S Manpage.s_description;
        `P "$(tname)";
        `S Manpage.s_bugs;
        `P "Bug reports to <pierre.villemot@ocamlpro.com>";
      ]
    in
    let info = Cmd.info "getrusage" ~version:"dev" ~doc ~man in
    Cmd.v info Term.(const main $ show_output $ prog $ args)

  let main () = exit (Cmd.eval cmd)
end

let () = Cmd.main ()
