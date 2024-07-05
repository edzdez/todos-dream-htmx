let string_of_elt elt = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) elt

let root _request =
  Dream.html
  @@ string_of_elt
  @@
  let open Tyxml.Html in
  html (head (title (txt "Todos")) []) (body [ h1 [ txt "Todos" ] ])
;;

let () = Dream.run @@ Dream.logger @@ Dream.router [ Dream.get "/" root ]
