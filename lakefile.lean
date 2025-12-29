import Lake
open Lake DSL

package tincture where
  version := v!"0.1.0"

require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.1"
require staple from git "https://github.com/nathanial/staple" @ "v0.0.1"

require plausible from git
  "https://github.com/leanprover-community/plausible.git" @ "v4.26.0"

@[default_target]
lean_lib Tincture where
  roots := #[`Tincture]

lean_lib TinctureTests where
  globs := #[.submodules `TinctureTests]

@[test_driver]
lean_exe tests where
  root := `TinctureTests.Main
