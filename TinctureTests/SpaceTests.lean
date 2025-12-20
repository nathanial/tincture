/-
  Tests for color space conversions.
-/

import Tincture
import Crucible

namespace TinctureTests.SpaceTests

open Crucible
open Tincture

/-- Helper to check if two floats are approximately equal. -/
def approxEq (a b : Float) (epsilon : Float := 0.01) : Bool :=
  Float.abs (a - b) < epsilon

/-- Helper to check if two colors are approximately equal. -/
def colorApproxEq (c1 c2 : Color) (epsilon : Float := 0.01) : Bool :=
  approxEq c1.r c2.r epsilon &&
  approxEq c1.g c2.g epsilon &&
  approxEq c1.b c2.b epsilon

testSuite "HSL Color Space"

test "red has hue 0" := do
  let hsl := HSL.fromColor Color.red
  ensure (approxEq hsl.h 0.0) "red hue should be 0"

test "red has full saturation" := do
  let hsl := HSL.fromColor Color.red
  ensure (approxEq hsl.s 1.0) "red saturation should be 1"

test "red has 0.5 lightness" := do
  let hsl := HSL.fromColor Color.red
  ensure (approxEq hsl.l 0.5) "red lightness should be 0.5"

test "green has hue 1/3" := do
  let hsl := HSL.fromColor Color.green
  ensure (approxEq hsl.h (1.0/3.0)) "green hue should be 1/3"

test "blue has hue 2/3" := do
  let hsl := HSL.fromColor Color.blue
  ensure (approxEq hsl.h (2.0/3.0)) "blue hue should be 2/3"

test "white has lightness 1" := do
  let hsl := HSL.fromColor Color.white
  ensure (approxEq hsl.l 1.0) "white lightness should be 1"

test "black has lightness 0" := do
  let hsl := HSL.fromColor Color.black
  ensure (approxEq hsl.l 0.0) "black lightness should be 0"

test "gray has zero saturation" := do
  let hsl := HSL.fromColor (Color.gray 0.5)
  ensure (approxEq hsl.s 0.0) "gray saturation should be 0"

test "HSL roundtrip preserves red" := do
  let roundtrip := HSL.toColor (HSL.fromColor Color.red) 1.0
  ensure (colorApproxEq roundtrip Color.red) "roundtrip should preserve red"

test "HSL roundtrip preserves green" := do
  let roundtrip := HSL.toColor (HSL.fromColor Color.green) 1.0
  ensure (colorApproxEq roundtrip Color.green) "roundtrip should preserve green"

test "HSL roundtrip preserves blue" := do
  let roundtrip := HSL.toColor (HSL.fromColor Color.blue) 1.0
  ensure (colorApproxEq roundtrip Color.blue) "roundtrip should preserve blue"

testSuite "HSV Color Space"

test "red has hue 0 in HSV" := do
  let hsv := HSV.fromColor Color.red
  ensure (approxEq hsv.h 0.0) "red hue should be 0"

test "red has full saturation in HSV" := do
  let hsv := HSV.fromColor Color.red
  ensure (approxEq hsv.s 1.0) "red saturation should be 1"

test "red has full value in HSV" := do
  let hsv := HSV.fromColor Color.red
  ensure (approxEq hsv.v 1.0) "red value should be 1"

test "HSV roundtrip preserves color" := do
  let original := Color.rgb 0.8 0.4 0.2
  let roundtrip := HSV.toColor (HSV.fromColor original) 1.0
  ensure (colorApproxEq roundtrip original) "roundtrip should preserve color"

testSuite "OkLab Color Space"

test "white has L close to 1 in OkLab" := do
  let oklab := OkLab.fromColor Color.white
  ensure (approxEq oklab.l 1.0) "white L should be ~1"

test "black has L close to 0 in OkLab" := do
  let oklab := OkLab.fromColor Color.black
  ensure (approxEq oklab.l 0.0) "black L should be ~0"

test "gray is neutral in OkLab" := do
  let oklab := OkLab.fromColor (Color.gray 0.5)
  ensure (approxEq oklab.a 0.0 0.02) "gray a should be ~0"
  ensure (approxEq oklab.b 0.0 0.02) "gray b should be ~0"

test "OkLab roundtrip preserves red" := do
  let roundtrip := OkLab.toColor (OkLab.fromColor Color.red)
  ensure (colorApproxEq roundtrip Color.red 0.02) "roundtrip should preserve red"

test "OkLab roundtrip preserves cyan" := do
  let roundtrip := OkLab.toColor (OkLab.fromColor Color.cyan)
  ensure (colorApproxEq roundtrip Color.cyan 0.02) "roundtrip should preserve cyan"

testSuite "OkLCH Color Space"

test "OkLCH roundtrip preserves colors" := do
  let original := Color.rgb 0.6 0.3 0.8
  let roundtrip := OkLCH.toColor (OkLCH.fromColor original) 1.0
  ensure (colorApproxEq roundtrip original 0.02) "roundtrip should preserve color"

testSuite "CIE Lab Color Space"

test "white has L=100 in Lab" := do
  let lab := Lab.fromColor Color.white
  ensure (approxEq lab.l 100.0 1.0) "white L should be ~100"

test "black has L=0 in Lab" := do
  let lab := Lab.fromColor Color.black
  ensure (approxEq lab.l 0.0) "black L should be 0"

test "Lab roundtrip preserves colors" := do
  let original := Color.rgb 0.4 0.6 0.2
  let roundtrip := Lab.toColor (Lab.fromColor original)
  ensure (colorApproxEq roundtrip original 0.02) "roundtrip should preserve color"

testSuite "CIE XYZ Color Space"

test "XYZ roundtrip preserves colors" := do
  let original := Color.rgb 0.5 0.7 0.3
  let roundtrip := XYZ.toColor (XYZ.fromColor original)
  ensure (colorApproxEq roundtrip original 0.02) "roundtrip should preserve color"

test "white has Y=1 in XYZ" := do
  let xyz := XYZ.fromColor Color.white
  ensure (approxEq xyz.y 1.0 0.05) "white Y should be ~1"

testSuite "CMYK Color Space"

test "white is 0% CMYK" := do
  let cmyk := CMYK.fromColor Color.white
  ensure (approxEq cmyk.c 0.0) "white C should be 0"
  ensure (approxEq cmyk.m 0.0) "white M should be 0"
  ensure (approxEq cmyk.y 0.0) "white Y should be 0"
  ensure (approxEq cmyk.k 0.0) "white K should be 0"

test "black is 100% K" := do
  let cmyk := CMYK.fromColor Color.black
  ensure (approxEq cmyk.k 1.0) "black K should be 1"

test "red has magenta and yellow" := do
  let cmyk := CMYK.fromColor Color.red
  ensure (approxEq cmyk.c 0.0) "red C should be 0"
  ensure (approxEq cmyk.m 1.0) "red M should be 1"
  ensure (approxEq cmyk.y 1.0) "red Y should be 1"
  ensure (approxEq cmyk.k 0.0) "red K should be 0"

test "CMYK roundtrip preserves colors" := do
  let original := Color.rgb 0.6 0.3 0.8
  let roundtrip := CMYK.toColor (CMYK.fromColor original)
  ensure (colorApproxEq roundtrip original 0.02) "roundtrip should preserve color"

testSuite "HWB Color Space"

test "white has 100% whiteness" := do
  let hwb := HWB.fromColor Color.white
  ensure (approxEq hwb.w 1.0) "white W should be 1"
  ensure (approxEq hwb.b 0.0) "white B should be 0"

test "black has 100% blackness" := do
  let hwb := HWB.fromColor Color.black
  ensure (approxEq hwb.b 1.0) "black B should be 1"

test "HWB roundtrip preserves colors" := do
  let original := Color.rgb 0.8 0.3 0.5
  let roundtrip := HWB.toColor (HWB.fromColor original) 1.0
  ensure (colorApproxEq roundtrip original 0.02) "roundtrip should preserve color"

testSuite "Linear RGB"

test "linearize and delinearize are inverses" := do
  let original := Color.rgb 0.5 0.6 0.7
  let linear := LinearRGB.fromColor original
  let back := LinearRGB.toColor linear
  ensure (colorApproxEq back original 0.01) "roundtrip should preserve color"

test "linear values are less than sRGB for mid-tones" := do
  let linear := LinearRGB.fromColor (Color.gray 0.5)
  ensure (linear.r < 0.5) "linear 0.5 should be less than sRGB 0.5"

#generate_tests

end TinctureTests.SpaceTests
