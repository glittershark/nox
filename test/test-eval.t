  $ nox eval -expr 'null'
  null

  $ nox eval -expr 'let z = 7; in (x: y: x + y + z) (1 + 1) (2 + 2)'
  13

Attribute sets, using nix syntax:

  $ nox eval -expr '{}'
  { }

  $ nox eval -expr '{ x = 1; y = 2; }'
  { x = 1; y = 2; }

Attribute names may be quoted, for names that aren't identifiers:

  $ nox eval -expr '{ "not an ident" = 1; }'
  { "not an ident" = 1; }

Attribute sets nest, and their values are arbitrary expressions:

  $ nox eval -expr '{ outer = { inner = 1 + 1; }; }'
  { outer = { inner = 2; }; }

  $ nox eval -expr '{ outer.inner = 1 + 1; }'
  { outer = { inner = 2; }; }

  $ nox eval -expr '{ outer.inner = 1 + 1; outer.inner2 = 4; }'
  { outer = { inner = 2; inner2 = 4; }; }

  $ nox eval -expr 'let attrs = { x = 1; }; in attrs'
  { x = 1; }
