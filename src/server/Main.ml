open Backend
open Frontend

(** {2 Embedded web server} *)
module Cmd = struct
  let main
      ?(local_only=false) ?port ~allow_delete
      (defs:Definitions.t) () =
    try
      let addr = if local_only then "127.0.0.1" else "0.0.0.0" in
      let server = H.create ~max_connections:32 ~addr ?port () in
      let data_dir = Misc.data_dir () in
      let self = {
        defs; server; data_dir; task_q=Task_queue.create ~defs ();
        meta_cache=Hashtbl.create ~random:true 16;
        allow_delete;
      } in
      (* thread to execute tasks *)
      let _th_r = Thread.create Task_queue.loop self.task_q in
      (* trick: see if debug level is active *)
      Log.debug (fun k ->
          H._enable_debug true;
          k "enable http debug"
        );
      (* maybe serve the API *)
      Printf.printf "listen on http://localhost:%d/\n%!" (H.port server);
      Handlers.handle_root self;
      Handlers.handle_list_benchs self;
      Handlers.handle_file_summary self;
      Handlers.handle_css server;
      Handlers.handle_show self;
      Handlers.handle_show_gp self;
      Handlers.handle_prover_in self;
      Handlers.handle_show_errors self;
      Handlers.handle_show_invalid self;
      Handlers.handle_show_as_table self;
      Handlers.handle_show_detailed self;
      Handlers.handle_show_single self;
      Handlers.handle_show_csv self;
      Handlers.handle_tasks self;
      Handlers.handle_provers self;
      Handlers.handle_run self;
      Handlers.handle_job_interrupt self;
      Handlers.handle_compare self;
      if allow_delete then Handlers.handle_delete self;
      Handlers.handle_file self;
      H.run server |> CCResult.map_err Error.of_exn
    with e ->
      Error (Error.of_exn e)

  (* sub-command to serve the web UI *)
  let cmd =
    let open Cmdliner in
    let port =
      Arg.(value & opt (some int) None & info ["p";"port"] ~doc:"port to listen on")
    and local_only =
      Arg.(value & flag & info ["local-only"] ~doc:"only listen on localhost")
    and allow_delete =
      Arg.(value & opt bool false & info ["allow-delete"] ~doc:"allow deletion of files")
    and defs =
      Bin_utils.definitions_term
    in
    let doc = "serve embedded web UI on given port" in
    let aux defs port local_only allow_delete () =
      main ?port ~local_only ~allow_delete defs () in
    Term.(const aux $ defs $ port $ local_only $ allow_delete $ const () ),
    Cmd.info ~doc "serve"
end

let () =
  CCFormat.set_color_default true;
  if Sys.getenv_opt "PROFILE"=Some "1" then Profile.enable();
  let eval (t, i) =
    Cmdliner.Cmd.eval_value (Cmdliner.Cmd.v i t)
  in
  match Profile.with1 "cmdliner" eval Cmd.cmd with
  | Error (`Parse | `Term | `Exn) -> exit 2
  | Ok (`Ok (Ok ()) | `Version | `Help) -> ()
  | Ok `Ok (Error e) ->
    print_endline ("error: " ^ Error.show e);
    exit 1
