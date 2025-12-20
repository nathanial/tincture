/-
  Property-based tests for Tincture.
  These tests verify invariants that should hold for all valid inputs.
-/

import Tincture
import Crucible

namespace TinctureTests.PropertyTests

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

testSuite "Property: Clamp01"

test "clamp01 output is always in [0, 1]" := do
  let values := #[-1.0, -0.5, 0.0, 0.25, 0.5, 0.75, 1.0, 1.5, 2.0, 100.0]
  for v in values do
    let clamped := Color.clamp01 v
    ensure (clamped >= 0.0 && clamped <= 1.0) "clamped should be in [0,1]"

test "clamp01 is idempotent" := do
  let values := #[-1.0, 0.0, 0.5, 1.0, 2.0]
  for v in values do
    let once := Color.clamp01 v
    let twice := Color.clamp01 once
    once ≡ twice

testSuite "Property: Lerp"

test "lerp at t=0 returns first color for many colors" := do
  let colors := #[Color.red, Color.green, Color.blue, Color.white, Color.black]
  for c1 in colors do
    for c2 in colors do
      let result := Color.lerp c1 c2 0.0
      ensure (colorApproxEq result c1) "lerp at 0 should return first"

test "lerp at t=1 returns second color for many colors" := do
  let colors := #[Color.red, Color.green, Color.blue, Color.white, Color.black]
  for c1 in colors do
    for c2 in colors do
      let result := Color.lerp c1 c2 1.0
      ensure (colorApproxEq result c2) "lerp at 1 should return second"

testSuite "Property: Contrast Ratio"

test "contrast ratio is symmetric" := do
  let colors := #[Color.red, Color.green, Color.blue, Color.white, Color.black, Color.gray 0.5]
  for c1 in colors do
    for c2 in colors do
      let r1 := Color.contrastRatio c1 c2
      let r2 := Color.contrastRatio c2 c1
      ensure (approxEq r1 r2) "contrast should be symmetric"

test "contrast ratio is always >= 1" := do
  let colors := #[Color.red, Color.green, Color.blue, Color.white, Color.black, Color.gray 0.3, Color.gray 0.7]
  for c1 in colors do
    for c2 in colors do
      let ratio := Color.contrastRatio c1 c2
      ensure (ratio >= 1.0) "ratio should be >= 1"

test "contrast ratio is always <= 21" := do
  let colors := #[Color.red, Color.green, Color.blue, Color.white, Color.black]
  for c1 in colors do
    for c2 in colors do
      let ratio := Color.contrastRatio c1 c2
      ensure (ratio <= 21.0) "ratio should be <= 21"

testSuite "Property: Color Space Roundtrips"

test "HSL roundtrip preserves many colors" := do
  let colors := #[Color.red, Color.green, Color.blue, Color.cyan, Color.magenta, Color.yellow]
  for c in colors do
    let roundtrip := HSL.toColor (HSL.fromColor c) c.a
    ensure (colorApproxEq roundtrip c 0.02) "HSL roundtrip should preserve"

test "OkLab roundtrip preserves many colors" := do
  let colors := #[Color.red, Color.green, Color.blue, Color.white, Color.black]
  for c in colors do
    let roundtrip := OkLab.toColor (OkLab.fromColor c)
    ensure (colorApproxEq roundtrip c 0.02) "OkLab roundtrip should preserve"

testSuite "Property: Luminance"

test "luminance is always in [0, 1]" := do
  let colors := #[Color.red, Color.green, Color.blue, Color.white, Color.black, Color.yellow, Color.cyan, Color.magenta]
  for c in colors do
    let lum := Color.relativeLuminance c
    ensure (lum >= 0.0 && lum <= 1.0) "luminance should be in [0,1]"

testSuite "Property: Blend Modes"

test "multiply with white is identity" := do
  let colors := #[Color.red, Color.green, Color.blue, Color.gray 0.5]
  for c in colors do
    let result := Color.multiply c Color.white
    ensure (colorApproxEq result c) "multiply with white should be identity"

test "screen with black is identity" := do
  let colors := #[Color.red, Color.green, Color.blue, Color.gray 0.5]
  for c in colors do
    let result := Color.screen c Color.black
    ensure (colorApproxEq result c) "screen with black should be identity"

test "difference of color with itself is black" := do
  let colors := #[Color.red, Color.green, Color.blue, Color.white, Color.gray 0.5]
  for c in colors do
    let diff := Color.blend .difference c c
    ensure (approxEq diff.r 0.0) "diff r should be 0"
    ensure (approxEq diff.g 0.0) "diff g should be 0"
    ensure (approxEq diff.b 0.0) "diff b should be 0"

testSuite "Property: Hex Roundtrip"

test "hex roundtrip preserves many colors" := do
  let colors := #[Color.red, Color.green, Color.blue, Color.white, Color.black]
  for c in colors do
    match Color.fromHex (Color.toHex c) with
    | some parsed => ensure (colorApproxEq parsed c 0.01) "roundtrip should preserve"
    | none => ensure false "parsing failed"

testSuite "Property: Harmony"

test "complementary of complementary is original" := do
  let colors := #[Color.red, Color.green, Color.blue, Color.yellow]
  for c in colors do
    let double := Color.complementary (Color.complementary c)
    let origHsl := HSL.fromColor c
    let doubleHsl := HSL.fromColor double
    ensure (approxEq origHsl.h doubleHsl.h 0.02) "double complement should match"

testSuite "Property: Palette Generation"

test "qualitative palette colors are valid" := do
  let palette := Palette.qualitative 10
  for c in palette do
    ensure (c.r >= 0.0 && c.r <= 1.0) "r should be valid"
    ensure (c.g >= 0.0 && c.g <= 1.0) "g should be valid"
    ensure (c.b >= 0.0 && c.b <= 1.0) "b should be valid"
    ensure (c.a >= 0.0 && c.a <= 1.0) "a should be valid"

test "random palette colors are valid" := do
  let palette := Palette.random 10
  for c in palette do
    ensure (c.r >= 0.0 && c.r <= 1.0) "r should be valid"
    ensure (c.g >= 0.0 && c.g <= 1.0) "g should be valid"
    ensure (c.b >= 0.0 && c.b <= 1.0) "b should be valid"
    ensure (c.a >= 0.0 && c.a <= 1.0) "a should be valid"

testSuite "Property: Gradient"

test "gradient samples are valid colors" := do
  let grad := Gradient.linear Color.red Color.blue
  let samples := grad.sample 10
  for c in samples do
    ensure (c.r >= 0.0 && c.r <= 1.0) "r should be valid"
    ensure (c.g >= 0.0 && c.g <= 1.0) "g should be valid"
    ensure (c.b >= 0.0 && c.b <= 1.0) "b should be valid"

test "gradient sample count is correct" := do
  let grad := Gradient.linear Color.red Color.blue
  let sizes := #[0, 1, 5, 10, 100]
  for n in sizes do
    let samples := grad.sample n
    samples.size ≡ n

testSuite "Property: CMYK Roundtrip"

test "CMYK roundtrip preserves many colors" := do
  let colors := #[Color.red, Color.green, Color.blue, Color.white, Color.cyan, Color.magenta, Color.yellow]
  for c in colors do
    let roundtrip := CMYK.toColor (CMYK.fromColor c)
    ensure (colorApproxEq roundtrip c 0.02) "CMYK roundtrip should preserve"

testSuite "Property: Delta E"

test "deltaE of color with itself is zero" := do
  let colors := #[Color.red, Color.green, Color.blue, Color.white, Color.black]
  for c in colors do
    let de := Color.deltaE2000 c c
    ensure (approxEq de 0.0 0.001) "deltaE with self should be 0"

test "deltaE is symmetric" := do
  let colors := #[Color.red, Color.green, Color.blue]
  for c1 in colors do
    for c2 in colors do
      let de1 := Color.deltaE2000 c1 c2
      let de2 := Color.deltaE2000 c2 c1
      ensure (approxEq de1 de2 0.001) "deltaE should be symmetric"

test "deltaE is always non-negative" := do
  let colors := #[Color.red, Color.green, Color.blue, Color.white, Color.black, Color.gray 0.5]
  for c1 in colors do
    for c2 in colors do
      let de := Color.deltaE2000 c1 c2
      ensure (de >= 0.0) "deltaE should be >= 0"

#generate_tests

end TinctureTests.PropertyTests
