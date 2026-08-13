(lang dune 3.24)
(name graphic-calc)
(authors "Aram Brouwer")
(using menhir 3.0)
(package
  (name graphic-calc)
  (synopsis "Graphical Calculator")
  (depends
    (raylib (>= 2.2.1))
    (menhir (>= 3.0))))
