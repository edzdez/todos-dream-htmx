let string_of_elt elt = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) elt

module TodoItem = struct
  type t =
    { id : int
    ; title : string
    ; description : string
    }
  [@@deriving yojson]

  type ts = t list [@@deriving yojson]

  type create_request =
    { title : string
    ; description : string
    }
  [@@deriving yojson]

  type update_request =
    { title : (string[@default ""])
    ; description : (string[@default ""])
    }
  [@@deriving yojson]

  module Persistence = struct
    module T = Caqti_type
    open Caqti_request.Infix

    let todo =
      let encode { id; title; description } = Ok (id, title, description) in
      let decode (id, title, description) = Ok { id; title; description } in
      let rep = T.tup3 T.int T.string T.string in
      T.custom ~encode ~decode rep
    ;;

    let create =
      let query =
        (T.tup2 T.string T.string ->! todo)
        @@ "INSERT INTO todos(title, description) VALUES (?, ?) RETURNING *"
      in
      fun ({ title; description } : create_request) (module Db : Caqti_lwt.CONNECTION) ->
        let%lwt todo = Db.find query (title, description) in
        Caqti_lwt.or_fail todo
    ;;

    let get_all =
      let query = (T.unit ->* todo) @@ "SELECT * FROM todos" in
      fun (module Db : Caqti_lwt.CONNECTION) ->
        let%lwt todos = Db.collect_list query () in
        Caqti_lwt.or_fail todos
    ;;

    let get =
      let query = (T.int ->? todo) @@ "SELECT * FROM todos WHERE id = ?" in
      fun id (module Db : Caqti_lwt.CONNECTION) ->
        let%lwt todo = Db.find_opt query id in
        Caqti_lwt.or_fail todo
    ;;

    let update =
      let query =
        (T.tup3 T.string T.string T.int ->. T.unit)
        @@ "UPDATE todos SET title = ?, description = ? WHERE id = ?"
      in
      fun { id; title; description } (module Db : Caqti_lwt.CONNECTION) ->
        let%lwt unit = Db.exec query (title, description, id) in
        Caqti_lwt.or_fail unit
    ;;

    let remove =
      let query = (T.int ->. T.unit) @@ "DELETE FROM todos WHERE id = ?" in
      fun id (module Db : Caqti_lwt.CONNECTION) ->
        let%lwt unit = Db.exec query id in
        Caqti_lwt.or_fail unit
    ;;
  end

  let update (todo : t) (request : update_request) =
    { id = todo.id
    ; title = (if request.title = "" then todo.title else request.title)
    ; description =
        (if request.description = "" then todo.description else request.description)
    }
  ;;
end

let get_all_todos request =
  let%lwt todos = Dream.sql request TodoItem.Persistence.get_all in
  Dream.json @@ Yojson.Safe.to_string @@ TodoItem.ts_to_yojson todos
;;

let get_todo request =
  let id = Dream.param request "id" in
  match int_of_string_opt id with
  | None -> Dream.empty `Bad_Request
  | Some id ->
    let%lwt maybe_todo = Dream.sql request (TodoItem.Persistence.get id) in
    (match maybe_todo with
     | None -> Dream.empty `Not_Found
     | Some todo -> Dream.json @@ Yojson.Safe.to_string @@ TodoItem.to_yojson todo)
;;

let create_todo request =
  match Dream.header request "Content-Type" with
  | Some "application/json" ->
    let%lwt body = Dream.body request in
    (match TodoItem.create_request_of_yojson @@ Yojson.Safe.from_string body with
     | Error e ->
       Dream.error (fun log ->
         log ~request "Failed to parse TodoItem.create_request with error: %s" e);
       Dream.empty `Bad_Request
     | Ok create_request ->
       let%lwt todo = Dream.sql request (TodoItem.Persistence.create create_request) in
       Dream.json
         ~status:`Created
         ~headers:[ "Location", Printf.sprintf "/api/todos/%d" todo.id ]
       @@ Yojson.Safe.to_string
       @@ TodoItem.to_yojson todo)
  | _ -> Dream.empty `Bad_Request
;;

let update_todo request =
  let id = Dream.param request "id" in
  match int_of_string_opt id with
  | None -> Dream.empty `Bad_Request
  | Some id ->
    let%lwt todo = Dream.sql request (TodoItem.Persistence.get id) in
    (match todo with
     | None -> Dream.empty `Not_Found
     | Some todo ->
       (match Dream.header request "Content-Type" with
        | Some "application/json" ->
          let%lwt body = Dream.body request in
          (match TodoItem.update_request_of_yojson @@ Yojson.Safe.from_string body with
           | Error e ->
             Dream.error (fun log ->
               log ~request "Failed to parse TodoItem.update_request with error: %s" e);
             Dream.empty `Bad_Request
           | Ok update_request ->
             let todo = TodoItem.update todo update_request in
             let%lwt () = Dream.sql request (TodoItem.Persistence.update todo) in
             Dream.json
               ~status:`OK
               ~headers:[ "Content-Location", Printf.sprintf "/api/todos/%d" id ]
             @@ Yojson.Safe.to_string
             @@ TodoItem.to_yojson todo)
        | _ -> Dream.empty `Bad_Request))
;;

let delete_todo request =
  let id = Dream.param request "id" in
  match int_of_string_opt id with
  | None -> Dream.empty `Bad_Request
  | Some id ->
    let%lwt todo = Dream.sql request (TodoItem.Persistence.get id) in
    (match todo with
     | None -> Dream.empty `Not_Found
     | Some _ ->
       let%lwt () = Dream.sql request (TodoItem.Persistence.remove id) in
       Dream.empty `No_Content)
;;

let root _request =
  Dream.html
  @@ string_of_elt
  @@
  let open Tyxml.Html in
  html (head (title (txt "Todos")) []) (body [ h1 [ txt "Todos" ] ])
;;

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.sql_pool "sqlite3:sqlite.db"
  @@ Dream.memory_sessions
  @@ Dream.router
       [ Dream.get "/" root
       ; Dream.scope
           "/api"
           []
           [ Dream.get "/todos" get_all_todos
           ; Dream.get "/todos/:id" get_todo
           ; Dream.post "/todos" create_todo
           ; Dream.patch "/todos/:id" update_todo
           ; Dream.delete "/todos/:id" delete_todo
           ]
       ]
;;
