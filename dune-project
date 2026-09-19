(lang dune 3.23)

(name quadlet-check)

(generate_opam_files true)

(source
 (github bartbes/quadlet-check))

(authors "Bart <bartbes@gmail.com>")

(maintainers "Bart <bartbes@gmail.com>")

(license MIT)

(package
 (name quadlet-check)
 (synopsis "Check podman quadlet files for image updates")
 (description "Quadlet-check parses your podman quadlet files, and checks if the images they refer to have updates")
 (depends ocaml))
