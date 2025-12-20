import Lake
open Lake DSL

package tincture where
  version := v!"0.1.0"

require crucible from git
  "https://github.com/nathanial/crucible.git" @ "master"

@[default_target]
lean_lib Tincture where
  roots := #[`Tincture]

lean_lib TinctureTests where
  globs := #[.submodules `TinctureTests]

@[test_driver]
lean_exe tests where
  root := `TinctureTests.Main
