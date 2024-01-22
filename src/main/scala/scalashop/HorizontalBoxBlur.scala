package scalashop

import org.scalameter.*

import java.util.concurrent.ForkJoinTask
import scala.collection.parallel.ForkJoinTasks

object HorizontalBoxBlurRunner:

  val standardConfig = config(
    Key.exec.minWarmupRuns := 5,
    Key.exec.maxWarmupRuns := 10,
    Key.exec.benchRuns := 10,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val radius = 3
    val width = 1920
    val height = 1080
    val src = Img(width, height)
    val dst = Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")

/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur extends HorizontalBoxBlurInterface:

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit =
    for (x <- 0 until src.width) {
      for (y <- from until end) {
        dst(x, y) = boxBlurKernel(src, x, y, radius)
      }
    }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit =
    val jobRange = src.width / numTasks
    var tasks = List[ForkJoinTask[Unit]]()
    for (i <- 0 until numTasks) {
      val from = clamp(i * jobRange, 0, src.height)
      val end = clamp((i + 1) * jobRange, 0, src.height)
      blur(src, dst, from, end, radius)
      val t = task(blur(src, dst, from, end, radius))
      tasks = t :: tasks
    }
    runTasks(tasks)


  private def runTasks(tasks: List[ForkJoinTask[Unit]]): Unit =
    tasks.foreach(
      (t: ForkJoinTask[Unit]) => t.join()
    )
