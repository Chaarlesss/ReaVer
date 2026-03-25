(lang dune 3.22)

(generate_opam_files true)

(license ISC)

(package
 (name reaver)
 (version 0.9.4)
 (license "GPL-3.0")
 (authors "Peter Shrammel")
 (maintainers "Charles de Haro <deharo@di.ens.fr>")
 (synopsis "ReaVer is a tool framework for safety verification of discrete and hybrid systems")
 (source
  (github Chaarlesss/ReaVer))
 (depends
  (ocaml
   (>= 5.4))
  dune
  camllib
  bddapron
  (apron (>= 0.9.15))
  fixpoint
  (odoc :with-doc)
  (alcotest :with-test)))
