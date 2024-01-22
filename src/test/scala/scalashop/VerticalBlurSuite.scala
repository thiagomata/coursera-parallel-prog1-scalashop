package scalashop

import java.util.concurrent.*
import scala.collection.*
class VerticalBlurSuite extends munit.FunSuite:

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

  test("blur 1 column") {
    val dest = new Img(4, 4, Array(
      p01, p02, p03, p04,
      p05, p06, p07, p08,
      p09, p10, p11, p12,
      p13, p14, p15, p16,
    ))
    VerticalBoxBlur.blur(
      src=src,dst=dest,
      from=0,
      end=1,
      radius = 1
    )

    val n01 = boxBlurKernel(src, 0, 0, 1)
    val n05 = boxBlurKernel(src, 0, 1, 1)
    val n09 = boxBlurKernel(src, 0, 2, 1)
    val n13 = boxBlurKernel(src, 0, 3, 1)


    val expectedImg = Img(4, 4, Array(
      n01, p02, p03, p04,
      n05, p06, p07, p08,
      n09, p10, p11, p12,
      n13, p14, p15, p16,
    ))

    assertEqualsImg(dest, expectedImg)
  }

  test("blur 2 columns") {
    val dest = new Img(4, 4, Array(
      p01, p02, p03, p04,
      p05, p06, p07, p08,
      p09, p10, p11, p12,
      p13, p14, p15, p16,
    )
    )
    VerticalBoxBlur.blur(
      src = src, dst = dest,
      from = 0,
      end = 2,
      radius = 1
    )

    val n01 = boxBlurKernel(src, 0, 0, 1)
    val n05 = boxBlurKernel(src, 0, 1, 1)
    val n09 = boxBlurKernel(src, 0, 2, 1)
    val n13 = boxBlurKernel(src, 0, 3, 1)
    val n02 = boxBlurKernel(src, 1, 0, 1)
    val n06 = boxBlurKernel(src, 1, 1, 1)
    val n10 = boxBlurKernel(src, 1, 2, 1)
    val n14 = boxBlurKernel(src, 1, 3, 1)


    val expectedImg = Img(4, 4, Array(
      n01, n02, p03, p04,
      n05, n06, p07, p08,
      n09, n10, p11, p12,
      n13, n14, p15, p16,
    ))

    assertEqualsImg(dest, expectedImg)
  }

  test("blur parallel 2 columns") {
    val dest = new Img(4, 4, Array(
      p01, p02, p03, p04,
      p05, p06, p07, p08,
      p09, p10, p11, p12,
      p13, p14, p15, p16,
    )
    )
    VerticalBoxBlur.parBlur(
      src = src, dst = dest,
      numTasks = 2,
      radius = 1
    )

    val n01 = boxBlurKernel(src, 0, 0, 1)
    val n02 = boxBlurKernel(src, 1, 0, 1)
    val n03 = boxBlurKernel(src, 2, 0, 1)
    val n04 = boxBlurKernel(src, 3, 0, 1)
    val n05 = boxBlurKernel(src, 0, 1, 1)
    val n06 = boxBlurKernel(src, 1, 1, 1)
    val n07 = boxBlurKernel(src, 2, 1, 1)
    val n08 = boxBlurKernel(src, 3, 1, 1)
    val n09 = boxBlurKernel(src, 0, 2, 1)
    val n10 = boxBlurKernel(src, 1, 2, 1)
    val n11 = boxBlurKernel(src, 2, 2, 1)
    val n12 = boxBlurKernel(src, 3, 2, 1)
    val n13 = boxBlurKernel(src, 0, 3, 1)
    val n14 = boxBlurKernel(src, 1, 3, 1)
    val n15 = boxBlurKernel(src, 2, 3, 1)
    val n16 = boxBlurKernel(src, 3, 3, 1)


    val expectedImg = Img(4, 4, Array(
      n01, n02, n03, n04,
      n05, n06, n07, n08,
      n09, n10, n11, n12,
      n13, n14, n15, n16,
    ))

    assertEqualsImg(dest, expectedImg)
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
