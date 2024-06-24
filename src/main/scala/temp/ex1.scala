package temp

import cats.effect.IOApp
import cats.effect.IO
import cats.implicits.*
import cats.syntax.all.*
import scala.collection.mutable.ArrayBuffer

object Ex1 extends IOApp.Simple:
  def ex1UpdateList(zx: Int, zu: Int): List[(Int, Int)] =
    (for
      i <- 1 to zx
      (j, a, b) <- Iterator
        .iterate((1, i + 1, i * i)) { case (j, a, b) =>
          (j + 1, a + 1, b + 1)
        }
        .takeWhile { case (j, _, _) => j <= zx }
      z <- j to zu by 2
    yield List((z, a), (z + 1, b))).flatten.toList

  def doLoops: IO[ArrayBuffer[Int]] =
    for
      array <- IO(ArrayBuffer.fill(4)(0))
      _ <- ex1UpdateList(4, 3).traverse_ { case (idx, newValue) =>
        IO(array.update(idx - 1, newValue))
      }
    yield array

  def arrayToList(array: ArrayBuffer[Int]): IO[List[Int]] = 
    IO(array.toList)

  def compute: IO[List[Int]] = 
    for
      array <- doLoops
      list <- arrayToList(array)
    yield list

  def run: IO[Unit] = 
    for
      result <- compute
      _ <- IO.println(result)
    yield ()
