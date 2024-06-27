(lang dune 3.16)

(name risc_v)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name risc_v)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune hardcaml ppx_hardcaml hardcaml_waveterm)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
