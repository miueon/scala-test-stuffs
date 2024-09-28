package effect.free

import par.actor.Par
import cats.Monad
import scala.util.Try
// import effect.free.Console.readLn
import java.util.concurrent.Executor
import java.util.concurrent.Executors
import cats.syntax.all.*
import cats.MonadThrow
import fs2.Stream

infix type ~>[F[_], G[_]] = [x] => F[x] => G[x]

enum Free[+F[_], +A]:
  case Return(a: A) extends Free[Nothing, A]
  case Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case FlatMap[F[_], A, B](
      s: Free[F, A],
      f: A => Free[F, B]
  ) extends Free[F, B]
  case Error(t: Throwable) extends Free[Nothing, Nothing]
  case Handle[+F[_], +A](source: Free[F, A], handler: Throwable => Free[F, A]) extends Free[F, A]

  def flatMap[F2[x] >: F[x], B](f: A => Free[F2, B]): Free[F2, B] =
    Stream(1, 2, 3)
    FlatMap(this, f)

  def handleErrorWith[F2[x] >: F[x], A2 >: A](handler: Throwable => Free[F2, A2]): Free[F2, A2] = Handle(this, handler)

  def map[B](f: A => B): Free[F, B] =
    flatMap(a => Return(f(a)))

  def union[G[_]]: Free[[x] =>> F[x] | G[x], A] = this
  def covary[F2[x] >: F[x]]: Free[F2, A] = this

  def run[F2[x] >: F[x]: MonadThrow, A2 >: A]: F2[A2] = step.flatMap { 
    case Error(t)              => t.raiseError
    case Return(a) => a.pure
    case Suspend(fa) => fa.asInstanceOf[F2[A2]]
    case FlatMap(Suspend(fa), f) =>
      fa.asInstanceOf[F2[A]]
        .flatMap(a => f.asInstanceOf[A => Free[F2, A]](a).run)
    case FlatMap(Error(t), _) => t.raiseError
    case Handle(Suspend(fa), g) => 
      fa.asInstanceOf[F2[A2]].handleErrorWith(t => g(t).run)
    case Handle(Error(t), handler) => handler(t).run
    case FlatMap(_, _) =>
      sys.error("Impossible, since `step` eliminates these cases")
    case Handle(_, _) => 
      sys.error("Impossible, since `step` eliminates these cases")
  }

  // @annotation.tailrec
  final def step[F2[x] >: F[x]: MonadThrow, A2 >: A]: F2[Free[F2, A2]] = this match
    case FlatMap(FlatMap(x, f), g) =>
      x.flatMap(a => f(a).flatMap(y => g(y).covary[F])).step
    case FlatMap(Return(x), f) => f(x).step
    case Handle(source, handler) =>
      source match
        case Handle(s2, g) => 
          s2.handleErrorWith(x => g(x).handleErrorWith(handler)).step
        case Return(x) => Return(x).pure.handleErrorWith(t => handler(t).step)
        // TODO
        case _ => this.pure
        // TODO 
    case _ => this.pure

  // def runFree[G[_]: MonadThrow](t: F ~> G): G[A] =
  //   step match
  //     case Return(a)  => G.pure(a)
  //     case Suspend(s) => t(s)
  //     case FlatMap(x, f) =>
  //       x match
  //         case Suspend(resume) => t(resume).flatMap(a => f(a).covary[F].runFree[G](t))
  //         case _               => sys.error("Impossible, since step eliminates these cases")

  // def translate[G[_]](fToG: F ~> G): Free[G, A] =
  //   runFree([x] => (fx: F[x]) => Suspend(fToG(fx)))
    // runFree returns `GG` here is Free[G, A]
end Free

type Async[A] = Free[Par, A]
type TailRec[A] = Free[Function0, A]

object Free:
  given freeMonad[F[_]]: Monad[[x] =>> Free[F, x]] with
    def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
      fa.flatMap(f)
    def pure[A](x: A): Free[F, A] = Return(x)
    def tailRecM[A, B](a: A)(f: A => Free[F, Either[A, B]]): Free[F, B] =
      f(a).flatMap {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => Return(b)
      }

  given function0Monad: Monad[Function0] with
    def pure[A](x: A): () => A = () => x
    def flatMap[A, B](fa: () => A)(f: A => () => B): () => B = () => f(fa())()
    def tailRecM[A, B](a: A)(f: A => () => Either[A, B]): () => B =
      f(a)() match
        case Left(a)      => tailRecM(a)(f)
        case Right(value) => () => value

  // extension [A](fa: TailRec[A])
  //   @annotation.tailrec
  //   def runTrampoline: A = fa match
  //     case Return(a)  => a
  //     case Suspend(r) => r()
  //     case FlatMap(sub, k) =>
  //       sub match
  //         case Return(a)  => k(a).runTrampoline
  //         case Suspend(r) => k(r()).runTrampoline
  //         case FlatMap(y, g) =>
  //           y.flatMap(a => g(a).flatMap(k)).runTrampoline
end Free

import scala.io.StdIn.*
enum Console[A]:
  case ReadLine extends Console[Option[String]]
  case PrintLine(line: String) extends Console[Unit]

  def toPar: Par[A] = this match
    case ReadLine        => Par.lazyUnit(Try(readLine()).toOption)
    case PrintLine(line) => Par.lazyUnit(println(line))

  def toThunk: () => A = this match
    case ReadLine        => () => Try(readLine()).toOption
    case PrintLine(line) => () => println(line)

// object Console:
//   def readLn: Free[Console, Option[String]] =
//     Free.Suspend(ReadLine)

//   def printLn(line: String): Free[Console, Unit] =
//     Free.Suspend(PrintLine(line))

//   extension [A](fa: Free[Console, A])
//     def unsafeRunConsole: A =
//       fa.translate([x] => (c: Console[x]) => c.toThunk).runTrampoline

// enum Files[A]:
//   case ReadLines(file: String) extends Files[List[String]]
//   case WriteLines(file: String, lines: List[String]) extends Files[Unit]

//   def toPar: Par[A] = this match
//     case ReadLines(file) =>
//       Par.lazyUnit(scala.io.Source.fromFile(file).getLines().toList)
//     case WriteLines(file, lines) =>
//       ???

//   def toThunk: () => A = ???

// object Files:
//   def readLines(file: String): Free[Files, List[String]] =
//     Free.Suspend(Files.ReadLines(file))

//   def writeLines(file: String, lines: List[String]): Free[Files, Unit] =
//     Free.Suspend(Files.WriteLines(file, lines))

//   def cat(file: String) =
//     Files.readLines(file).union[Console].flatMap { lines =>
//       Console.printLn(lines.mkString("\n")).union[Files]
//     }

// @main
// def test =
//   val f1 =
//     for
//       _ <- Console.printLn("Test println")
//       ln <- readLn
//       _ <- ln match
//         case Some(v) => Console.printLn(v)
//         case None    => Console.printLn("n")
//     yield ln

//   val v = f1.runFree([t] => (x: Console[t]) => x.toPar).run

//   println(v(Executors.newFixedThreadPool(10)))
