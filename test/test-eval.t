  $ nox eval -expr 'null'
  Null

  $ nox eval -expr 'let z = 7; in (x: y: x + y + z) (1 + 1) (2 + 2)'
  (Integer 13)

Attribute sets, using nix syntax:

  $ nox eval -expr '{}'
  (Attrs ((attrs ())))

  $ nox eval -expr '{ x = 1; y = 2; }'
  (Attrs ((attrs ((x (Thunk <thunk>)) (y (Thunk <thunk>))))))

Attribute names may be quoted, for names that aren't identifiers:

  $ nox eval -expr '{ "not an ident" = 1; }'
  (Attrs ((attrs (("not an ident" (Thunk <thunk>))))))

Attribute sets nest, and their values are arbitrary expressions:

  $ nox eval -expr '{ outer = { inner = 1 + 1; }; }'
  (Attrs ((attrs ((outer (Thunk <thunk>))))))

  $ nox eval -expr 'let attrs = { x = 1; }; in attrs'
  (Attrs ((attrs ((x (Thunk <thunk>))))))
