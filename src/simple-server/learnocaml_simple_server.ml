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

let static_dir = ref (Filename.concat (Sys.getcwd ()) "www")

let sync_dir = ref (Filename.concat (Sys.getcwd ()) "sync")

let auth_file () = Filename.concat !sync_dir "auth.json"

let port = ref 8080

let args = Arg.align @@
  [ "-static-dir", Arg.Set_string static_dir,
    "PATH where static files should be found (./www)" ;
    "-sync-dir", Arg.Set_string sync_dir,
    "PATH where sync tokens are stored (./sync)" ;
    "-port", Arg.Set_int port,
    "PORT the TCP port (8080)" ]

type teacher = {
  teacher_login: string;
  teacher_password: string;
}

type auth = {
  mutable one_time_token: string option;
  mutable teachers: teacher list;
}

open Lwt.Infix

let read_static_file path =
  let shorten path =
    let rec resolve acc = function
      | [] -> List.rev acc
      | "." :: rest -> resolve acc rest
      | ".." :: rest ->
          begin match acc with
            | [] -> resolve [] rest
            | _ :: acc -> resolve acc rest end
      | name :: rest -> resolve (name :: acc) rest in
    resolve [] path in
  let path =
    String.concat Filename.dir_sep (!static_dir :: shorten path) in
  Lwt_io.(with_file ~mode: Input path (fun chan -> read chan))

let retrieve token =
  Lwt.catch (fun () ->
      let path =
        Filename.concat !sync_dir
          Learnocaml_sync.Token.(to_path (parse token)) in
      Lwt_io.(with_file ~mode: Input path (fun chan -> read chan)))
  @@ function
  | Unix.Unix_error (Unix.ENOENT, _, _) -> raise Not_found
  | e -> raise e


let check_save_file contents =
  try
    let json = Ezjsonm.from_string contents in
    let _save = Json_encoding.destruct Learnocaml_sync.save_file_enc json in
    (* Should we do more test ?? *)
    true
  with _ -> false

let create_token_file token =
  let path = Filename.concat !sync_dir (Learnocaml_sync.Token.to_path token) in
  Lwt_utils.mkdir_p ~perm:0o700 (Filename.dirname path) >>= fun () ->
  Lwt_io.(with_file ~mode: Output ~perm:0o700 path (fun chan -> write chan ""))

let store token contents =
  let token = Learnocaml_sync.Token.parse token in
  let path =
    Filename.concat !sync_dir
      (Learnocaml_sync.Token.to_path token) in
  (if not (Sys.file_exists path) then create_token_file token
   else Lwt.return_unit) >>= fun _ ->
  Lwt_io.(with_file ~mode: Output path (fun chan -> write chan contents))

let rec gimme () =
  let token = Learnocaml_sync.Token.random () in
  if Sys.file_exists (Learnocaml_sync.Token.to_path token) then
    gimme ()
  else
    create_token_file token >|= fun () ->
    Learnocaml_sync.Token.to_string token

let auth_encoding =
  let open Json_encoding in
  let teacher_encoding =
    obj2
      (req "login" string)
      (req "password" string)
    |> conv
      (fun {teacher_login; teacher_password} ->
         teacher_login, teacher_password)
      (fun (teacher_login, teacher_password) ->
         {teacher_login; teacher_password})
  in
  list teacher_encoding |> conv
    (fun { one_time_token = _; teachers } -> teachers)
    (fun teachers -> { one_time_token = None; teachers })

let read_auth file =
  let open Lwt_io in
  with_file ~mode:Input file @@ fun ic ->
  read ic >|= fun str ->
  (Ezjsonm.from_string str |>
   Json_encoding.destruct auth_encoding)

let write_auth file auth =
  let open Lwt_io in
  with_file ~mode:Output file @@ fun oc ->
  Json_encoding.construct auth_encoding auth |> function
  | `O _ | `A _ as json ->
      write oc (Ezjsonm.to_string json)
  | _ -> assert false

let get_auth ?(url="URL") () =
  let f = auth_file () in
  Lwt_unix.file_exists f >>= function
  | true -> read_auth f
  | false ->
      let rand _ = String.get alphabet (Random.int (String.length alphabet)) in
      let token = String.init 18 rand in
      let auth = {
        one_time_token = Some token;
        teachers = [];
      } in
      write_auth f auth >|= fun () ->
      Printf.printf
        "Use %s/first-login/%s to initialise a teacher account.\n%!"
        url token;
      auth

exception Too_long_body

let string_of_stream ?(max_size = 64 * 1024) s =
  let b = Bytes.create max_size in
  let pos = ref 0 in
  let add_string s =
    let len = String.length s in
    pos := !pos + len ;
    if !pos > max_size then
      Lwt.fail Too_long_body
    else begin
      String.blit s 0 b (!pos - len) len ;
      Lwt.return_unit
    end
  in
  Lwt.catch begin function () ->
    Lwt_stream.iter_s add_string s >>= fun () ->
    Lwt.return (Some (Bytes.sub_string b 0 !pos))
  end begin function
    | Too_long_body -> Lwt.return None
    | e -> Lwt.fail e
  end

let launch () =
  let open Lwt in
  let open Cohttp_lwt_unix in
  let callback auth _ req body =
    let path = Uri.path (Request.uri req) in
    let path = Stringext.split ~on:'/' path in
    let path = List.filter ((<>) "") path in
    let respond_static path =
      catch
        (fun () ->
           read_static_file path >>= fun body ->
           let headers =
             try
               Cohttp.Header.init_with "Content-Type"
                 (Magic_mime.lookup (List.fold_left (fun _ r -> r) "" path))
             with _ -> Cohttp.Header.init () in
           Server.respond_string ~headers ~status:`OK ~body ())
        (fun _ ->
           Server.respond_not_found ()) in
    let cookies = Cohttp.Cookie.Cookie_hdr.extract (Cohttp.Request.headers req) in
    match Request.meth req, path with
    | `GET, [] ->
        respond_static [ "index.html" ]
    | `GET, [ "sync" ; "gimme" ] ->
        gimme () >>= fun token ->
        let body = Printf.sprintf "{\"token\":\"%s\"}" token in
        Server.respond_string ~status:`OK ~body ()
    | `GET, [ "sync" ; token ] when Learnocaml_sync.Token.check token ->
        (Lwt.catch
           (fun () ->
              retrieve token >>= fun body ->
              Server.respond_string ~status:`OK ~body ())
         @@ function
         | Not_found -> Server.respond_string ~status:`OK ~body:"" ()
         | e -> raise e)
    | `POST, [ "sync" ; token ] when Learnocaml_sync.Token.check token -> begin
        string_of_stream (Cohttp_lwt.Body.to_stream body) >>= function
        | None ->
            Server.respond_string ~status:`Bad_request ~body: "Too much data" ()
        | Some body ->
            if check_save_file body then
              store token body >>= fun () ->
              Server.respond_string ~status:`OK ~body: "Stored." ()
            else
              Server.respond_string ~status:`Bad_request ~body: "Invalid save file" ()
      end
    | `GET, ["first-login"; token] ->
        if Some token = auth.one_time_token then
          (auth.one_time_token <- None;
           Server.respond_string ~status:`OK ~body: "Logged in!" ())
        else
          Server.respond_error ~status:`Forbidden
            ~body: "Invalid credentials" ()
    | `GET, ("teacher" :: _ as path) -> assert false
(*
        let validate_cookie _ =
          Cohttp.Request.resource req = auth.admin_token
        in
        (match validate_cookie (List.assoc "teacher_token" cookies) with
         | true ->
             Server.respond_string ~status:`OK ~body: "Teacher alright." ()
         | false ->
             Server.respond_error ~status:`Forbidden
               ~body: "Invalid credentials" ()
         | exception (Not_found | Failure _) ->
             Server.respond_error ~status:`Forbidden
               ~body: "Authentication required" ())
*)

(*
    | `POST, [ "tokens_list" ] ->
        (match admin_auth with
         | None ->
             Server.respond_error ~status:`Not_found
               ~body: "No admin credentials configured"
         | Some (login, pass) ->
             (Server.respond_need_auth ~auth:(`Basic "Is this my realm ?") ()
              >>= fun (resp, _body) ->
              match Cohttp.Header.get_authorization (Cohttp.Response.headers resp) with
              | None -> Server.respond_error ~status:`Forbidden
                          ~body: "Authentication required" ()
              | Some a -> Server.respond_static [ "index.html" ]

         match Cohttp.Auth.credential_of_string body
when passwd = Digest.to_hex ->
        
*)
    | `GET, path -> respond_static path
    | _ -> Server.respond_error ~status: `Bad_request ~body: "Bad request" () in
  Random.self_init () ;
  get_auth () >>= fun auth ->
  let callback = callback auth in
  Lwt.catch (fun () ->
      Server.create
        ~on_exn: (function
            | Unix.Unix_error(Unix.EPIPE, "write", "") -> ()
            | exn -> raise exn)
        ~mode:(`TCP (`Port !port)) (Server.make ~callback ()) >>= fun () ->
      Lwt.return true)
  @@ function
  | Sys.Break ->
      Lwt.return true
  | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
      Printf.eprintf
        "Could not bind port %d, another instance may still be running?\n%!"
        !port;
      Lwt.return false
  | e ->
      Printf.eprintf "Server error: %s\n%!" (Printexc.to_string e);
      Lwt.return false
