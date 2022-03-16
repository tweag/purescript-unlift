{ name = "unlift"
, license = "MIT"
, repository = "https://github.com/tweag/purescript-unlift.git"
, dependencies =
  [ "aff"
  , "effect"
  , "either"
  , "identity"
  , "lists"
  , "maybe"
  , "monad-control"
  , "prelude"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
