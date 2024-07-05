let string_of_elt elt = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) elt

module TodoItem = struct
  type t =
    { id : int
    ; title : string
    ; description : string
    }
  [@@deriving yojson]

  type ts = t list [@@deriving yojson]

  type request =
    { title : string
    ; description : string
    }
  [@@deriving yojson]

  let store : ts ref =
    ref
      [ { id = 0; title = "Hello"; description = "World" }
      ; { id = 1; title = "Test"; description = "Todo" }
      ]
  ;;

  let store_lock = Mutex.create ()
  let get_all () = Mutex.protect store_lock (fun _ -> !store)

  let get id =
    Mutex.protect store_lock (fun _ -> List.find_opt (fun todo -> todo.id = id) !store)
  ;;

  let create request =
    Mutex.protect store_lock (fun _ ->
      let id = List.length !store in
      let todo = { id; title = request.title; description = request.description } in
      store := todo :: !store;
      todo)
  ;;
end

let get_all_todos _request =
  Dream.json @@ Yojson.Safe.to_string @@ TodoItem.ts_to_yojson @@ TodoItem.get_all ()
;;

let get_todo request =
  let id = Dream.param request "id" in
  match int_of_string_opt id with
  | None -> Dream.empty `Bad_Request
  | Some id ->
    (match TodoItem.get id with
     | None -> Dream.empty `Not_Found
     | Some todo -> Dream.json @@ Yojson.Safe.to_string @@ TodoItem.to_yojson todo)
;;

let create_todo request =
  match Dream.header request "Content-Type" with
  | Some "application/json" ->
    let%lwt body = Dream.body request in
    (match TodoItem.request_of_yojson @@ Yojson.Safe.from_string body with
     | Error e ->
       Dream.error (fun log ->
         log ~request "Failed to parse Todo Request with error %s" e);
       Dream.empty `Bad_Request
     | Ok request ->
       let todo = TodoItem.create request in
       Dream.json
         ~status:`Created
         ~headers:[ "Location", Printf.sprintf "/api/todos/%d" todo.id ]
       @@ Yojson.Safe.to_string
       @@ TodoItem.to_yojson todo)
  | _ -> Dream.empty `Bad_Request
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
  @@ Dream.router
       [ Dream.get "/" root
       ; Dream.scope
           "/api"
           []
           [ Dream.get "/todos" get_all_todos
           ; Dream.get "/todos/:id" get_todo
           ; Dream.post "/todos" create_todo
           ]
       ]
;;
