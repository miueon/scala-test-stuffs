package effect

import java.util.concurrent.ExecutorService
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal
import par.actor.Par
import cats.Monad
import cats.syntax.all.*

/*
 * `Task[A]` is an opaque type around `IO[Try[A]]` which is
 * an opaque type around `Free[Par, Try[A]]`, with some
 * convenience functions for handling exceptions.
 */
// opaque type Task[A] = IO[Try[A]]
// object Task:
//   extension [A](self: Task[A])

//     /* 'Catches' exceptions in the given task and returns them as values. */
//     def attempt: Task[Try[A]] =
//       IO.monad.map(self):
//         case Failure(e) => Success(Failure(e))
//         case Success(a) => Success(Success(a))

//     def handleErrorWith(h: Throwable => Task[A]): Task[A] =
//       attempt.flatMap:
//         case Failure(t) => h(t)
//         case Success(a) => Task.now(a)

//     def or[B >: A](t2: Task[B]): Task[B] =
//       IO.monad.flatMap(self):
//         case Failure(e) => t2
//         case a          => IO(a)

//     def unsafeRunSync(es: ExecutorService): A = IO.unsafeRunSync(self)(es).get

//     def unsafeAttemptRunSync(es: ExecutorService): Try[A] =
//       try IO.unsafeRunSync(self)(es)
//       catch case NonFatal(t) => Failure(t)

//   def apply[A](a: => A): Task[A] = IO(Try(a))

//   def raiseError[A](e: Throwable): Task[A] = IO(Failure(e))
//   def now[A](a: A): Task[A] = IO.now(Success(a))

//   def more[A](a: => Task[A]): Task[A] = now(()).flatMap(_ => a)

//   def delay[A](a: => A): Task[A] = more(apply(a))

//   def fork[A](a: => Task[A]): Task[A] =
//     IO.par(Par.lazyUnit(())).flatMap(_ => a)

//   def forkUnit[A](a: => A): Task[A] = fork(now(a))

//   given monad: Monad[Task] with
//     def pure[A](x: A): Task[A] = Task(x)
//     def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] =
//       IO.monad.flatMap(fa) {
//         case Failure(exception) => IO(Failure(exception))
//         case Success(value)     => f(value)
//       }
//     def tailRecM[A, B](a: A)(f: A => Task[Either[A, B]]): Task[B] =
//       f(a).flatMap {
//         case Left(a)  => tailRecM(a)(f)
//         case Right(b) => Task(b)
//       }
