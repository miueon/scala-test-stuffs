import zio.ZIOAppDefault
import zio.Console.*
import zio.Ref
import zio.ZIO
import zio.Task
import scala.collection.immutable.Queue
import zio.Promise

case class State[A](queue: Queue[A], capacity: Int, takers: Queue[Promise[Throwable, A]], offerers: Queue[(A, Promise[Throwable, Unit])])
object State: 
  def apply[A](capacity: Int, queue: Queue[A]): State[A] = State(queue, capacity, Queue.empty, Queue.empty)

object MyApp extends ZIOAppDefault:
  val logic = 
    for
      _ <- printLine("Hello, world!")
      _ <- printLine("What is your name?")
      name <- readLine
      _ <- printLine(s"Hello, $name!")
    yield ()

  def run = prodConsumerLogic.exitCode

  def producer(id: Int, counterR: Ref[Int], stateR: Ref[State[Int]]): Task[Unit] = 
    def offer(i: Int): Task[Unit] =
      for
        offerer <- Promise.make[Throwable, Unit]
        _ <- stateR.modify{
          case State(queue, capacity, takers, offerers) if takers.nonEmpty=> 
            val (taker, rest) = takers.dequeue
            taker.succeed(i).unit -> State(queue, capacity, rest, offerers)
          case State(queue, capacity, takers, offerers) if queue.size < capacity => 
            ZIO.unit -> State(queue.enqueue(i), capacity, takers, offerers)
          case State(queue, capacity, takers, offerers) => 
            offerer.await -> State(queue, capacity, takers, offerers.enqueue((i, offerer)))
        }.flatten
      yield ()

    for
      i <- counterR.getAndUpdate(_ + 1)
      _ <- offer(i)
      _ <- if i % 10000 == 0 then printLine(s"Producer $id produced $i items") else ZIO.none
      _ <- producer(id, counterR, stateR)
    yield ()

  def consumer(id: Int, stateR: Ref[State[Int]]): Task[Unit] = 
    val take = 
      (for
        taker <- Promise.make[Throwable, Int]
        result <- stateR.modify{
          case State(queue, capacity, takers, offerers) if queue.nonEmpty && offerers.isEmpty => 
            val (i, rest) =queue.dequeue
            ZIO.succeed(i) -> State(rest, capacity, takers, offerers) 
          case State(queue, capacity, takers, offerers) if queue.nonEmpty  => 
            val (i, rest) = queue.dequeue
            val ((move, release),tail) = offerers.dequeue
            release.succeed(()).as(i) -> State(queue, capacity, takers, tail)
          case State(queue, capacity, takers, offerers) if offerers.nonEmpty => 
            val ((i, release ), rest) = offerers.dequeue
            release.succeed(()).as(i) -> State(queue, capacity, takers, rest)
          case State(queue, capacity, takers, offerers) => 
            taker.await -> State(queue, capacity, takers.enqueue(taker), offerers)
        }
      yield result
      ).flatten
    for
      i <- take
      _ <- if i % 10000 == 0 then printLine(s"Consumer $id consumed $i items") else ZIO.none
      _ <- consumer(id, stateR)
    yield ()

  def prodConsumerLogic =
    for
      stateR <- Ref.make(State[Int](100, Queue.empty))
      counterR <- Ref.make(1)
      producers = List.range(1, 11).map(producer(_, counterR, stateR))
      consumers = List.range(1, 11).map(consumer(_, stateR))
      res <- ZIO.forkAll(producers ++ consumers)
      _ <- res.join
    yield ()


  
