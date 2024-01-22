package scalashop

import java.util.concurrent.*
import scala.collection.*

class BlurSuite extends munit.FunSuite:

  val p01: RGBA = rgba(1, 1, 1, 1)
  val p02: RGBA = rgba(2, 2, 2, 2)
  val p03: RGBA = rgba(3, 3, 3, 3)
  val p04: RGBA = rgba(4, 4, 4, 4)
  val p05: RGBA = rgba(5, 5, 5, 5)
  val p06: RGBA = rgba(6, 6, 6, 6)
  val p07: RGBA = rgba(7, 7, 7, 7)
  val p08: RGBA = rgba(8, 8, 8, 8)
  val p09: RGBA = rgba(9, 9, 9, 9)
  val p10: RGBA = rgba(10, 10, 10, 10)
  val p11: RGBA = rgba(11, 11, 11, 11)
  val p12: RGBA = rgba(12, 12, 12, 12)
  val p13: RGBA = rgba(13, 13, 13, 13)
  val p14: RGBA = rgba(14, 14, 14, 14)
  val p15: RGBA = rgba(15, 15, 15, 15)
  val p16: RGBA = rgba(16, 16, 16, 16)

  val src: Img = Img(4, 4, Array(
    p01, p02, p03, p04,
    p05, p06, p07, p08,
    p09, p10, p11, p12,
    p13, p14, p15, p16,
  ))

  // test cases for boxBlurKernel
  test("boxBlurKernel should correctly blur middle pixel when radius is 1") {
    assertEquals(boxBlurKernel(src, 1, 1, 1), avgPixels(List(
      p01, p02, p03,
      p05, p06, p07,
      p09, p10, p11,
    )))
    assertEquals(boxBlurKernel(src, 2, 2, 1), avgPixels(List(
      p06, p07, p08,
      p10, p11, p12,
      p14, p15, p16,
    )))
  }
  test("boxBlurKernel should correctly blur corner pixel when radius is 1") {
    assertEquals(boxBlurKernel(src, 0, 0, 1), avgPixels(List(
      p01, p02,
      p05, p06,
    )))
    assertEquals(boxBlurKernel(src, 3, 0, 1), avgPixels(List(
      p03, p04,
      p07, p08,
    )))
    assertEquals(boxBlurKernel(src, 3, 3, 1), avgPixels(List(
      p11, p12,
      p15, p16,
    )))
    assertEquals(boxBlurKernel(src, 0, 3, 1), avgPixels(List(
      p09, p10,
      p13, p14,
    )))
  }
  test("boxBlurKernel should correctly blur corner pixel when radius is 2") {
    assertEquals(boxBlurKernel(src, 3, 3, 2), avgPixels(
      List(
        p06, p07, p08,
        p10, p11, p12,
        p14, p15, p16,
      )))
  }
  test("boxBlurKernel should correctly blur middle pixel when radius is 1") {
    assertEquals(boxBlurKernel(src, 1, 1, 1), avgPixels(List(
      p01, p02, p03,
      p05, p06, p07,
      p09, p10, p11,
    )))
    assertEquals(boxBlurKernel(src, 2, 2, 1), avgPixels(List(
      p06, p07, p08,
      p10, p11, p12,
      p14, p15, p16,
    )))
  }
  test("boxBlurKernel should correctly blur corner pixel when radius is 0") {
    assertEquals(boxBlurKernel(src, 0, 0, 0), avgPixels(List(
      p01,
    )))
    assertEquals(boxBlurKernel(src, 3, 0, 0), avgPixels(List(
      p04,
    )))
    assertEquals(boxBlurKernel(src, 3, 3, 0), avgPixels(List(
      p16,
    )))
    assertEquals(boxBlurKernel(src, 0, 3, 0), avgPixels(List(
      p13,
    )))
  }

  def assertEqualsImg(received: Img, expected: Img) = {
    assertEquals(received.width, expected.width)
    assertEquals(received.height, expected.height)
    var same = true
    println("")
    for (y <- 0 until received.height) {
      for (x <- 0 until received.width) {
        val r = received(x, y)
        val e = expected(x, y)
        if (r != e) {
          same = false
          print(s" ❌ ")
        } else {
          print(s" ✅ ")
        }
      }
      println("")
    }
    assert(same, "images are not equal")
  }

  def avgPixels(pixels: List[RGBA]): RGBA = {
    var r = 0
    var g = 0
    var b = 0
    var a = 0
    for (pixel <- pixels) {
      r += red(pixel)
      g += green(pixel)
      b += blue(pixel)
      a += alpha(pixel)
    }
    rgba(r / pixels.length, g / pixels.length, b / pixels.length, a / pixels.length)
  }
