(lang dune 3.15)

(name todos_dream_htmx)

(generate_opam_files true)

(source
 (github edzdez/todos-dream-htmx))

(authors "Ethan Zhang")

(maintainers "Ethan Zhang")

(license LICENSE)

(package
 (name todos_dream_htmx)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune dream tyxml lwtppx yojson ppx_deriving_yojson caqti caqti-driver-sqlite3)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
