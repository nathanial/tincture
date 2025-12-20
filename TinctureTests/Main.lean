/-
  Tincture Test Suite
  Main entry point for running all tests.
-/

import TinctureTests.ColorTests
import TinctureTests.SpaceTests
import TinctureTests.BlendTests
import TinctureTests.ContrastTests
import TinctureTests.HarmonyTests
import TinctureTests.ParseFormatTests
import TinctureTests.PropertyTests
import Crucible

open Crucible

def main : IO UInt32 := do
  IO.println "Tincture Color Library Tests"
  IO.println "============================"
  IO.println ""

  let mut failed := false

  if (← runTests "Color Core" TinctureTests.ColorTests.cases) != 0 then failed := true
  if (← runTests "Color Spaces" TinctureTests.SpaceTests.cases) != 0 then failed := true
  if (← runTests "Blend Modes" TinctureTests.BlendTests.cases) != 0 then failed := true
  if (← runTests "WCAG Contrast" TinctureTests.ContrastTests.cases) != 0 then failed := true
  if (← runTests "Color Harmony" TinctureTests.HarmonyTests.cases) != 0 then failed := true
  if (← runTests "Parse/Format" TinctureTests.ParseFormatTests.cases) != 0 then failed := true
  if (← runTests "Properties" TinctureTests.PropertyTests.cases) != 0 then failed := true

  IO.println ""
  IO.println "============================"

  if failed then
    IO.println "Some tests failed!"
    return 1
  else
    IO.println "All tests passed!"
    return 0
