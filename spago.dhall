{ name = "unlift"
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
