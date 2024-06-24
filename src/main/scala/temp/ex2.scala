package temp

import cats.effect.IOApp
import cats.effect.IO
import scala.collection.mutable.ArrayBuffer
import cats.syntax.all.*

object ex2 extends IOApp.Simple:

  def ex1UpdateList(zx: Int, zu: Int): List[(Int, Int)] =
    (for
      i <- 1 to zx
      (j, a, b) <- Iterator
        .iterate((1, i + 1, i * i)) { case (j, a, b) => (j + 1, a + 1, b + 1) }
        .takeWhile { case (j, _, _) => j <= zx }
      z <- j to zu by 2
    yield List((z, a), (z + 1, b))).flatten.toList

  def doLoops: IO[ArrayBuffer[Int]] = for
    array <- IO(ArrayBuffer.fill(4)(0))
    _ <- ex1UpdateList(4, 3).traverse_ { case (idx, newValue) =>
      IO(array.update(idx - 1, newValue)) // Arrays are 0-indexed in Scala
    }
  yield array

  def prefetchArray(array: ArrayBuffer[Int]): List[Int] =
    array.toList

  def ex3: IO[Int] = for
    array <- doLoops
    prefetchArrayValues = prefetchArray(array)
    result = prefetchArrayValues.filter(_ >= 7).sum
  yield result

  def run: IO[Unit] = for
    result <- ex3
    _ <- IO(println(result))
  yield ()
