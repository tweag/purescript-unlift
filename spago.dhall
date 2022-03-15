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
  , "prelude"
  , "st"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
