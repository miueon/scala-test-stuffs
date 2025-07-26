package kyo

import java.io.IOException
import kyo.*
import kyo.kernel.ArrowEffect
import kyo.kernel.Boundary
import kyo.kernel.ContextEffect
import scala.annotation.targetName

abstract class Actor[+E, A, B]:
  def subject: Subject[A]
  def fiber: Fiber[Closed | E, B]
  def result(using Frame): B < (Async & Abort[Closed | E]) = fiber.get

export Actor.Subject
object Actor:
  trait Subject[A]:
    def send(msg: A): Unit < (Async & Abort[Closed])
    def ask[B](f: Subject[B] => A)(using Frame): B < (Async & Abort[Closed]) =
      for
        promise <- Promise.init[Nothing, B]
        replyTo: Subject[B] = r => promise.completeDiscard(Result.success(r))
        _ <- send(f(replyTo))
        result <- promise.get
      yield result

  object Subject:
    private val _noop: Subject[Any] = _ => {}

    def noop[A]: Subject[A] = _noop.asInstanceOf[Subject[A]]

  def self[A: Tag](using Frame): Subject[A] < Env[Subject[A]] = Env.get

  def resend[A: Tag](msg: A)(using Frame): Unit < (Abort[Closed] & Env[Subject[A]] & Async) =
    Env.use[Subject[A]](_.send(msg))

  inline def run[E, A, B: Flat, Ctx](
      behavior: B < (Poll[A] & Env[Subject[A]] & Abort[E | Closed] & Ctx)
  )(using
      pollTag: Tag[Poll[A]],
      emitTag: Tag[Emit[A]]
  ): Actor[E, A, B] < (Async & Ctx) =
    _run(behavior)

  private def _run[E, A: Tag, B: Flat, Ctx](
      behavior: B < (Poll[A] & Env[Subject[A]] & Abort[E | Closed] & Ctx)
  )(using Boundary[Ctx, IO & Abort[E | Closed]], Frame): Actor[E, A, B] < (IO & Ctx) =
    for
      mailbox <-
        Channel.init[A](Int.MaxValue, Access.MultiProducerSingleConsumer)
      _subject: Subject[A] = 
        msg => mailbox.put(msg)
      _consumer <-
        Async._run {
          Env.run(_subject) {
            IO.ensure(mailbox.close.unit) {
              Loop(behavior) {b =>
                Poll.runFirst(b).map{
                  case (Ack.Stop, cont) =>
                    Poll.run(Chunk.empty)(cont(Maybe.empty)).map(Loop.done)
                  case (_, cont) =>
                    mailbox.take.map(v => Loop.continue(cont(Maybe(v))))
                }
                }
            }
          }
        }
    yield new Actor[E, A, B]:
      def subject: Subject[A] = _subject
      def fiber: Fiber[Closed | E, B] = _consumer
end Actor
