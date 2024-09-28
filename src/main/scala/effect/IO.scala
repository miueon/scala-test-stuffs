package effect

import cats.Monad
import cats.syntax.all.*
import scala.io.StdIn.*
import effect.free.Free
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import par.gear.Par
import par.gear.Par.{*, given}

import gears.async.*
import cats.MonadThrow

// abstract class SubjectObserver:
//   type S >: Subject
//   type O <: Observer

//   trait Subject:
//     s : S =>
//     private var observers = List[O]()

//     def addObserver(o: O) = observers ::= o

//     def notifyAllOs() = observers.foreach(_.receiveUpdate(s))

//   given Subject with
//     override def addObserver(o: O): Unit = ???
//     override def notifyAllOs(): Unit = ???

//   trait Observer:
//     def receiveUpdate(s: S): Unit

// enum IO[A]:
//   case Return(a: A) // IO action has finished
//   case Suspend(resume: () => A) // want to execute some effect
//   case FlatMap[A, B](
//       sub: IO[A],
//       k: A => IO[B]
//   ) extends IO[B] // extend or continue  an existing computation
//   def flatMap[B](f: A => IO[B]): IO[B] =
//     FlatMap(this, f)

//   def map[B](f: A => B): IO[B] =
//     flatMap(a => Return(f(a)))
//   // def unsafeRun: A
//   // def map[B](f: A => B): IO[B] = new:
//   //   def unsafeRun: B =
//   //     f(self.unsafeRun)
//   // def flatMap[B](f: A => IO[B]): IO[B] = new:
//   //   def unsafeRun: B =
//   //     f(self.unsafeRun).unsafeRun
//   // def ++(io: IO[B]): IO = new:
//   //   def unsafeRun: Unit =
//   //     self.unsafeRun
//   //     io.unsafeRun

//   @annotation.tailrec
//   final def unsafeRun(): A = this match
//     case Return(a)       => a
//     case Suspend(resume) => resume()
//     case FlatMap(sub, k) =>
//       sub match
//         case Return(a)       => k(a).unsafeRun()
//         case Suspend(resume) => k(resume()).unsafeRun()
//         case FlatMap(y, g) =>
//           y.flatMap(a => g(a).flatMap(k)).unsafeRun()  // Monad associativity law

// object IO:
//   def apply[A](a: => A): IO[A] =
//     suspend(Return(a))

//   def suspend[A](ioa: => IO[A]): IO[A] =
//     Suspend(() => ioa).flatMap(identity)

//   given monad: Monad[IO] with
//     def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
//       fa.flatMap(f)
//     def pure[A](x: A): IO[A] =
//       IO(x)
//     def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] =
//       f(a).flatMap {
//         case Left(a)  => tailRecM(a)(f)
//         case Right(b) => IO(b)
//       }

opaque type IO[A] = Free[Par, A]

object IO:
  def now[A](a: A): IO[A] = Free.Return(a)

  def par[A](pa: Par[A]): IO[A] = Free.Suspend(pa)

  def apply[A](a: => A): IO[A] =
    par(Par.delay(a))

  def async[A](cb: (A => Unit) => Unit): IO[A] =
    fork(par(Par.async(cb)))

  def fork[A](a: => IO[A]): IO[A] = par(Par.lazyUnit(())).flatMap(_ => a)

  def forkUnit[A](a: => A): IO[A] = fork(now(a))

  def raiseError[A](e: Throwable): IO[A] = Free.Error(e)

  extension [A](ioa: IO[A])
    def unsafeRunSync(using Async): A =
      ioa.run(using Par.parMonadThrow).run

  given monad: MonadThrow[IO] with
    def pure[A](x: A): IO[A] = IO(x)
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] =
      f(a).flatMap {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => IO(b)
      }
    def handleErrorWith[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] = fa.handleErrorWith(f)
    def raiseError[A](e: Throwable): IO[A] = IO.raiseError(e)
end IO
trait IOApp:
  import gears.async.default.given
  def main(args: Array[String]): Unit =
    Async.blocking:
      pureMain(args.toList).unsafeRunSync

  def pureMain(args: List[String]): IO[Unit]

def PrintLine(msg: String): IO[Unit] =
  IO(println(msg))

def ReadLine: IO[String] = IO(readLine())

def fahrenheitToCelsius(f: Double) =
  (f - 32) * 5.0 / 9.0
def converter =
  for
    _ <- PrintLine("Enter a temperature in degrees:")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString())
  yield ()
