  $ nox eval -expr 'null'
  Null

  $ nox eval -expr 'let z = 7; in (x: y: x + y + z) (1 + 1) (2 + 2)'
  Line 1, characters 0-3:
  1 | let z = 7; in (x: y: x + y + z) (1 + 1) (2 + 2)
      ^^^
  Error: [parser] unexpected token
  
  [1]
