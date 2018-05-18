(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2016 OCamlPro.
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

let ( / ) = Filename.concat

let repo_dir = ref "."
let contents_dir = ref (Filename.dirname Sys.argv.(0)/".."/"share"/"learn-ocaml"/"www")
let dest_dir = ref ("."/"www")


let args = Arg.align @@
  [ "-repo", Arg.String (fun s ->
        repo_dir := s;
        Learnocaml_process_exercise_repository.exercises_dir := s/"exercises";
        Learnocaml_process_tutorial_repository.tutorials_dir := s/"tutorials"),
    Printf.sprintf
      "PATH repository root containing the exercises, tutorials and lessons \
       (default: [%s])"
      !repo_dir] @
  [ "-dest-dir", Arg.Set_string dest_dir,
    Printf.sprintf
      "PATH path to the exercise repository (default: [%s])"
      !dest_dir] @
  [ "-contents-dir", Arg.Set_string contents_dir,
    Printf.sprintf
      "PATH directory containing the base learn-ocaml app contents (default: \
       [%s])"
      !contents_dir] @
  Learnocaml_process_exercise_repository.args @
  Learnocaml_process_tutorial_repository.args

let copy_tree src dst =
  let cmd = Printf.sprintf "cp -PRpT %S %S" src dst in
  if Sys.command cmd <> 0 then failwith "Failed to copy base app contents"

let () =
  Arg.parse args
    (fun anon -> raise (Arg.Bad "unexpected anonymous argument"))
    (Printf.sprintf "Usage: %s [options]" Sys.argv.(0));
  Arg.parse args
    (fun anon -> raise (Arg.Bad "unexpected anonymous argument"))
    (Printf.sprintf "Usage: %s [options]" Sys.argv.(0));
  copy_tree !contents_dir !dest_dir;
  if Sys.file_exists (!repo_dir/"lessons") then
    copy_tree (!repo_dir/"lessons") !dest_dir;
  let ret = Lwt_main.run @@
    (Learnocaml_process_tutorial_repository.main !dest_dir >>= fun e_ret ->
     Learnocaml_process_exercise_repository.main !dest_dir >>= fun t_ret ->
     Lwt.return (e_ret && t_ret)) in
  exit (if ret then 0 else 1)
