(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
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

(** {2 Configuration options} *)

(** Should stdout / stderr of the grader be echoed *)
val display_std_outputs: bool ref

(** Should outputs of the grader be saved and where *)
val dump_outputs: string option ref

(** Should the reports be saved and where *)
val dump_reports: string option ref

(** Should the message from 'test.ml' be displayed on stdout ? *)
val display_callback: bool ref

(** Should compiler outcome be printed ? *)
val display_outcomes: bool ref

(** Should the tool grade a student file instead of 'solution.ml' ? *)
val grade_student: string option ref

(** Should each test be run with a specific timeout (in secs) ? *)
val individual_timeout: int option ref

(** Display reports to stderr *)
val display_reports: bool ref

(** Should the tool generate and dump a dependency graph of the exercises and where *)
val dump_dot: string option ref

(** {2 Functions} *)

(** Runs the grading process *)
val grade:
  ?print_result:bool -> ?dirname:string -> Learnocaml_exercise.t -> string option ->
  (unit, int) result Lwt.t

val grade_from_dir:
  ?print_result:bool -> string -> string option ->
  (unit, int) result Lwt.t
