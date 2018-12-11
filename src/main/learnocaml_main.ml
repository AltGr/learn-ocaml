(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2018 OCamlPro.
 *
 * Learn-OCaml is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Learn-OCaml is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

open Lwt.Infix

module StringSet = Set.Make(String)

let ( / ) = Filename.concat

let readlink f =
  let cwd = Sys.getcwd () in
  let f =
    try
      Sys.chdir (Filename.dirname f);
      Filename.concat (Sys.getcwd ()) (Filename.basename f)
    with Sys_error _ -> f
  in
  try Sys.chdir cwd; f
  with Sys_error _ -> Sys.chdir (Filename.get_temp_dir_name ()); f

module Args = struct
  open Cmdliner
  open Arg

  module CF = Learnocaml_config_file

  type command = Grade | Build | Serve

  let commands =
    value & pos_all (Arg.enum [
        "grade", Grade;
        "build", Build;
        "serve", Serve;
      ]) [Build; Serve] &
    info [] ~docs:"COMMANDS" ~docv:"COMMAND"

  let config_file =
    value & opt (some file) None &
    info ["config"; "c"] ~docv:"FILE" ~doc:
      (Printf.sprintf "Json file containing the configuration to use, instead \
                       of $(b,%s). See $(i,CONFIGURATION FILE)."
         CF.default_filename)

  let repo_dir =
    value & opt (some dir) None & info ["repo"] ~docv:"DIR" ~doc:
      "The path to the repository containing the exercises, lessons and \
       tutorials. Defaults to the current directory."

  let app_dir =
    value & opt (some string) None & info ["app-dir"; "o"] ~docv:"DIR" ~doc:
      "Directory where the app should be generated for the $(i,build) command, \
       and from where it is served by the $(i,serve) command. Defaults to \
       $(b,www/), below the current directory."

  module Grader = struct
    let info = info ~docs:"GRADER OPTIONS"

    let exercises =
      value & opt_all (list dir) [["."]] &
      info ["exercises";"e"] ~docv:"DIRS" ~doc:
        "Directories where to find the exercises to be graded \
         (comma-separated). Can be repeated."

    let output_json =
      value & opt (some dir) None & info ["output-json"] ~docv:"DIR" ~doc:
        "save the processed exercises in JSON format in `.json` files, \
         in the given directory."

    let grade_student =
      value & opt (some file) None &
      info ["grade-student";"s"] ~docv:"FILE" ~doc:
        "grade the given student file instead of 'solution.ml'"

    let display_outcomes =
      value & flag & info ["display-outcomes"] ~doc:
        "display the toplevel's outcomes"

    let quiet =
      value & flag & info ["quiet";"q"] ~doc:
        "Don't display grading progression messages"

    let display_std_outputs =
      value & flag & info ["display-stdouts"] ~doc:
        "display the toplevel's standard outputs"

    let dump_outputs =
      value & opt (some string) None &
      info ["dump-outputs"] ~docv:"PREFIX" ~doc:
        "save the outputs in files with the given prefix"

    let dump_reports =
      value & opt (some string) None &
      info ["dump-reports"] ~docv:"PREFIX" ~doc:
        "save the reports in files with the given prefix"

    let timeout =
      value & opt (some int) None & info ["timeout"] ~docv:"SECONDS" ~doc:
        "Limit every test to the given timeout"

    let verbose =
      value & flag & info ["verbose"; "v"] ~doc:
        "Display detailed grading reports to stdout"

    let dump_dot =
      value & opt (some string) None & info ["dump-dot"] ~doc:
        "Generates a dependency graph of the repository and dumps it into the \
         given file"

    type t = {
      exercises: string list;
      output_json: string option;
    }

    let term =
      let apply
          exercises
          output_json grade_student display_outcomes quiet
          display_std_outputs dump_outputs dump_reports timeout
          verbose dump_dot =
        let exercises = List.flatten exercises in
        Grader_cli.grade_student := grade_student;
        Grader_cli.display_outcomes := display_outcomes;
        Grader_cli.display_callback := not quiet;
        Grader_cli.display_std_outputs := display_std_outputs;
        Grader_cli.dump_outputs := dump_outputs;
        Grader_cli.dump_reports := dump_reports;
        Grader_cli.individual_timeout := timeout;
        Grader_cli.display_reports := verbose;
        Grader_cli.dump_dot := dump_dot;
        Learnocaml_process_exercise_repository.dump_outputs := dump_outputs;
        Learnocaml_process_exercise_repository.dump_reports := dump_reports;
        { exercises; output_json }
      in
      Term.(const apply
            $exercises $output_json $grade_student $display_outcomes
            $quiet $display_std_outputs $dump_outputs $dump_reports
            $timeout $verbose $dump_dot)
  end

  module Builder = struct
    let info = info ~docs:"BUILDER OPTIONS"

    let contents_dir =
      let default =
        readlink (Filename.dirname (Filename.dirname (Sys.executable_name))
                  /"share"/"learn-ocaml"/"www")
      in
      value & opt dir default & info ["contents-dir"] ~docv:"DIR" ~doc:
        "directory containing the base learn-ocaml app contents"

    let enable opt doc =
      value & vflag None [
        Some true, info ["enable-"^opt] ~doc:("Enable "^doc);
        Some false, info ["disable-"^opt] ~doc:("Disable "^doc);
      ]

    let try_ocaml = enable "tryocaml"
        "the 'TryOCaml' tab (enabled by default if the repository contains a \
         $(i,tutorials) directory)"

    let lessons = enable "lessons"
        "the 'Lessons' tab (enabled by default if the repository contains a \
         $(i,lessons) directory)"

    let exercises = enable "exercises"
        "the 'Exercises' tab (enabled by default if the repository contains an \
         $(i,exercises) directory)"

    let toplevel = enable "toplevel"
        "the 'Toplevel' tab (enabled by default)"

    let exercises_filtered =
      value & opt_all (list string) [[]] &
      info ["exercises-filtered"; "f"] ~docv:"DIRS" ~doc:
        "Exercises to build (comma-separated), instead of taking \
         the entire repository. Can be repeated."

    let jobs =
      value & opt (some int) None & info ["jobs";"j"] ~docv:"INT" ~doc:
        "Number of building jobs to run in parallel. NOTE: parallel builds are \
         experimental and may hang, use is discouraged unless for testing \
         purposes."

    type t = {
      contents_dir: string;
      try_ocaml: bool option;
      lessons: bool option;
      exercises: bool option;
      toplevel: bool option;
      login_welcome_text: string option;
      nickname_text: string option;
    }

    let apply
        repo_dir contents_dir try_ocaml lessons exercises toplevel
        exercises_filtered jobs
      =
      

    let term =
      Term.(const apply $repo_dir $contents_dir
            $try_ocaml $lessons $exercises $toplevel $exercises_filtered $jobs)

    let apply repo_dir contents_dir
        try_ocaml lessons exercises toplevel exercises_filtered jobs =
      Learnocaml_process_exercise_repository.exercises_dir :=
        repo_dir/"exercises";
      Learnocaml_process_exercise_repository.exercises_filtered :=
        Learnocaml_data.SSet.of_list (List.flatten exercises_filtered);
      Learnocaml_process_tutorial_repository.tutorials_dir :=
        repo_dir/"tutorials";
      Learnocaml_process_exercise_repository.n_processes := jobs;
      { contents_dir; try_ocaml; lessons; exercises; toplevel;
        login_welcome_text = None;
        nickname_text = None;
      }

  end

  module Server = struct
    include Learnocaml_server_args
    let info = info ~docs:"SERVER OPTIONS"
  end

  type t = {
    commands: command list;
    config_file: string option;
    app_dir: string;
    repo_dir: string;
    grader: Grader.t;
    builder: Builder.t;
    server: Server.t;
  }

  let term =
    let apply commands config_file app_dir repo_dir grader builder server =
      let config_file, conf = match config_file with
        | Some f -> config_file, CF.load f
        | None ->
            if Sys.file_exists CF.default_filename
            then CF.(Some default_filename, load default_filename)
            else None, CF.default
      in
      let ( +! ) o v = match o with None -> v | Some v -> v in
      let ( + ) opt cfg = match opt with None -> cfg | some -> some in
      {
        commands;
        config_file;
        app_dir = app_dir +! "www/";
        repo_dir = ".";
        grader;
        builder = Builder.{
            builder with
            try_ocaml = builder.try_ocaml + conf.CF.enable_tryocaml;
            lessons = builder.lessons + conf.CF.enable_lessons;
            exercises = builder.exercises + conf.CF.enable_exercises;
            toplevel = builder.toplevel + conf.CF.enable_toplevel;
            login_welcome_text = conf.CF.login_welcome_text;
            nickname_text = conf.CF.nickname_text;
          };
        server;
      }
    in
    Term.(const apply $commands $config_file $app_dir $repo_dir
          $Grader.term $Builder.term $Server.term app_dir)
end

open Args

let main o =
  Printf.printf "Learnocaml v.%s running.\n" Learnocaml_api.version;
  let grade () =
    if List.mem Grade o.commands then
      (if List.mem Build o.commands || List.mem Serve o.commands then
         failwith "The 'grade' command is incompatible with 'build' and \
                   'serve'";
       Lwt_list.fold_left_s (fun i ex ->
           let json_output = match o.grader.Grader.output_json with
             | None -> None
             | Some o ->
                 Some (Filename.concat o
                         (String.map (function '/' -> '_' | c -> c) ex
                          ^ ".json"))
           in
           Lwt.catch
             (fun () ->
                Grader_cli.grade_from_dir ~print_result:true ex json_output
                >|= function Ok () -> i | Error _ -> 1)
             (fun e ->
                Printf.ksprintf failwith
                  "Could not load exercise at %s: %s" ex (Printexc.to_string e)))
         0 o.grader.Grader.exercises
       >|= fun i -> Some i)
    else Lwt.return_none
  in
  let generate () =
    if List.mem Build o.commands then
      (Printf.printf "Updating app at %s\n%!" o.app_dir;
       Lwt.catch
         (fun () -> Lwt_utils.copy_tree o.builder.Builder.contents_dir o.app_dir)
         (function
           | Failure _ ->
               Lwt.fail_with @@ Printf.sprintf
                 "Failed to copy base app contents from %s"
                 (readlink o.builder.Builder.contents_dir)
           | e -> Lwt.fail e)
       >>= fun () ->
       let if_enabled opt dir f =
         (match opt with
          | None ->
              Lwt.catch (fun () ->
                  Lwt_unix.stat dir >|= fun st -> st.Unix.st_kind = Unix.S_DIR)
                (function Unix.Unix_error _ -> Lwt.return_false
                        | e -> Lwt.fail e)
          | Some opt -> Lwt.return opt)
         >>= fun enabled ->
         if enabled then f dir >>= Lwt.return_some else Lwt.return_none
       in
       if_enabled o.builder.Builder.lessons (o.repo_dir/"lessons")
         (fun dir ->
            Lwt_utils.copy_tree dir (o.app_dir/"lessons") >>= fun () ->
            Lwt_unix.rename (o.app_dir/"lessons"/"lessons.json") (o.app_dir/"lessons.json")
            >|= fun () -> true)
       >>= fun lessons_ret ->
       if_enabled o.builder.Builder.try_ocaml (o.repo_dir/"tutorials")
         (fun _ -> Learnocaml_process_tutorial_repository.main (o.app_dir))
       >>= fun tutorials_ret ->
       if_enabled o.builder.Builder.exercises (o.repo_dir/"exercises")
         (fun _ -> Learnocaml_process_exercise_repository.main (o.app_dir))
       >>= fun exercises_ret ->
       Lwt_io.with_file ~mode:Lwt_io.Output (o.app_dir/"js"/"learnocaml-config.js")
         (fun oc ->
            Lwt_io.fprintf oc
              "var learnocaml_config = {\n\
              \  enableTryocaml: %b,\n\
              \  enableLessons: %b,\n\
              \  enableExercises: %b,\n\
              \  enableToplevel: %b\n\
               }\n"
              (tutorials_ret <> None)
              (lessons_ret <> None)
              (exercises_ret <> None)
              (o.builder.Builder.toplevel <> Some false) >>= fun () ->
       Lwt.return (tutorials_ret <> Some false && exercises_ret <> Some false)))
    else
      Lwt.return true
  in
  let run_server () =
    if List.mem Serve o.commands then
      let native_server = Sys.executable_name ^ "-server" in
      if Sys.file_exists native_server then
        let server_args =
          let open Server in
          ("--app-dir="^o.app_dir) ::
          ("--sync-dir="^o.server.sync_dir) ::
          ("--port="^string_of_int o.server.port) ::
          (match o.server.cert with None -> [] | Some c -> ["--cert="^c])
        in
        Unix.execv native_server (Array.of_list (native_server::server_args))
      else
        Printf.printf "Starting server on port %d\n%!"
          !Learnocaml_server.port;
      Learnocaml_server.launch ()
    else
      Lwt.return true
  in
  let ret =
    Lwt_main.run
      (grade () >>= function
        | Some i -> Lwt.return i
        | None ->
            generate () >>= fun success ->
            if success then
              run_server () >>= fun r ->
              if r then Lwt.return 0 else Lwt.return 10
            else
              Lwt.return 1)
  in
  exit ret

let man = [
  `S "DESCRIPTION";
  `P "This program performs various tasks related to generating, serving and \
      administrating a learn-ocaml web-app.";
  `S "COMMANDS";
  `P "The $(i,COMMAND) argument may be one or more of the following. If no \
      command is specified, '$(b,build) $(b,serve)' is assumed.";
  `I ("$(b,grade)", "Runs the automatic grader on exercise solutions.");
  `I ("$(b,build)", "Generates the application based on a repository \
                     containing the lessons, tutorials and exercises (see \
                     $(b,REPOSITORY FORMAT)).");
  `I ("$(b,serve)", "Run a web-server providing access to the learn-ocaml app, \
                     as well as user file synchronisation.");
  `S "OPTIONS";
  `S "GRADER OPTIONS";
  `S "BUILDER OPTIONS";
  `S "SERVER OPTIONS";
  `S "CONFIGURATION FILE"
] @ Learnocaml_config_file.doc @ [
  `S "REPOSITORY FORMAT";
  `P "The repository specified by $(b,--repo) is expected to contain \
      sub-directories $(b,lessons), $(b,tutorials) and/or $(b,exercises).";
  `S "AUTHORS";
  `P "Learn OCaml is written by OCamlPro. Its main authors are Benjamin Canou, \
      Çağdaş Bozman and Grégoire Henry. It is licensed under the GNU Affero \
      General Public License version 3: any instance of the app must provide \
      its source code to its users.";
  `S "BUGS";
  `P "Bugs should be reported to \
      $(i,https://github.com/ocaml-sf/learn-ocaml/issues)";
]

let main_cmd =
  Cmdliner.Term.(const main $ Args.term),
  Cmdliner.Term.info
    ~man
    ~doc:"Learn-ocaml web-app manager"
    ~version:Learnocaml_api.version
    "learn-ocaml"

let () =
  match
    Cmdliner.Term.eval ~catch:false main_cmd
  with
  | exception Failure msg ->
      Printf.eprintf "[ERROR] %s\n" msg;
      exit 1
  | `Error _ -> exit 2
  | _ -> exit 0
