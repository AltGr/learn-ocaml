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

type config = {
  display_std_outputs: bool;
  (** Should stdout / stderr of the grader be echoed *)
  dump_outputs: string option;
  (** Should outputs of the grader be saved and where *)
  dump_reports: string option;
  (** Should the reports be saved and where *)
  display_callback: bool;
  (** Should the message from 'test.ml' be displayed on stdout ? *)
  display_outcomes: bool;
  (** Should compiler outcome be printed ? *)
  grade_student: string option;
  (** Should the tool grade a student file instead of 'solution.ml' ? *)
  individual_timeout: int option;
  (** Should each test be run with a specific timeout (in secs) ? *)
  display_reports: bool;
  (** Display reports to stderr *)
  dump_dot: string option;
  (** Should the tool generate and dump a dependency graph of the exercises and
      where *)
}

val default_config: config

val config: config ref

(** Runs the grading process *)
val grade:
  ?print_result:bool -> ?dirname:string -> Learnocaml_exercise.t -> string option ->
  (unit, int) result Lwt.t

val grade_from_dir:
  ?print_result:bool -> string -> string option ->
  (unit, int) result Lwt.t
