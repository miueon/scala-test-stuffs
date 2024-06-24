package effect.pull.test

import cats.instances.boolean.*
import cats.kernel.Monoid
import cats.syntax.all.*
import effect.*
import effect.Pull.*
import effect.Stream.*

import scala.util.chaining.scalaUtilChainingOps
val nonEmpty: Pipe[Nothing1, String, String] = _.filter(_.nonEmpty)

val lowerCase: Pipe[Nothing1, String, String] = _.map(_.toLowerCase)

val normalize: Pipe[Nothing1, String, String] = nonEmpty andThen lowerCase

val lines = Stream("Hello", "", "World!")

val normalized = lines.pipe(normalize)



// def exists[I](f: I => Boolean): Pipe[I, Boolean] =
//   src =>
//     src
//       .map(f)
//       .toPull
//       .tally(using Monoid.instance(false, (x, y) => x || y))
//       .toStream

// def fromIterator[O](itr: Iterator[O]): Stream[O] =
//   Pull
//     .unfold(itr) { itr =>
//       if itr.hasNext then Right((itr.next(), itr))
//       else Left(itr)
//     }
//     .void
//     .toStream

// def processFile[A](
//     file: java.io.File,
//     p: Pipe[String, A]
// )(using m: Monoid[A]): IO[A] = IO {
//   val source = scala.io.Source.fromFile(file)
//   try fromIterator(source.getLines).pipe(p).fold(m.empty)(m.combine)
//   finally source.close()
// }

// def count[A]: Pipe[A, Int] = _.toPull.count.void.toStream

// def checkFileForGt40K(file: java.io.File): IO[Boolean] =
//   processFile(file, count andThen exists(_ > 4000))(using
//     Monoid.instance[Boolean](false, _ || _)
//   )
// // val normalize = non

// @main
// def test =
//   // Stream.apply(1, 2, 3).map(_ + 1)
//   ???
