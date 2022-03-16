{ name = "unlift"
, license = "MIT"
, repository = "https://github.com/tweag/purescript-unlift.git"
, dependencies =
  [ "aff"
  , "effect"
  , "either"
  , "freet"
  , "identity"
  , "lists"
  , "maybe"
  , "monad-control"
  , "prelude"
  , "st"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
