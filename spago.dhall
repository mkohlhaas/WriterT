{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "psci-support", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
