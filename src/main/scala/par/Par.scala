package par

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable
import java.util.spi.TimeZoneNameProvider

opaque type Par[A] = ExecutorService => Future[A]

object Par:
  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(
        new Callable[A]:
          def call(): A = a(es).get
      )

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sequenceBalanced[A](pas: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    if pas.isEmpty then unit(IndexedSeq.empty)
    else if pas.size == 1 then pas.head.map(IndexedSeq(_))
    else
      val (l, r) = pas.splitAt(pas.size / 2)
      sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    sequenceBalanced(ps.toIndexedSeq).map(_.toList)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    val fbs = ps.map(asyncF(f))
    sequence(fbs)

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    fork:
      val pars: List[Par[List[A]]] =
        as.map(asyncF(a => if f(a) then List(a) else Nil))
      sequence(pars).map(_.flatten)

  extension [A](pa: Par[A])

    def map[B](f: A => B): Par[B] =
      pa.map2(unit(()))((a, _) => f(a))

    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      es =>
        new Future[C]:
          val futureA = pa(
            es
          ) // When the future is created, the futureA will immediately started
          val futureB = pb(es)
          @volatile private var cache: Option[C] = None
          def isDone(): Boolean = cache.isDefined
          def get(): C = get(Long.MaxValue, TimeUnit.NANOSECONDS)

          def get(timeout: Long, unit: TimeUnit): C =
            val timeoutNs = TimeUnit.NANOSECONDS.convert(timeout, unit)
            val started = System.nanoTime()
            val a = futureA.get(timeoutNs, TimeUnit.NANOSECONDS)
            val elapsed = System.nanoTime() - started
            val b = futureB.get(timeoutNs - elapsed, TimeUnit.NANOSECONDS)
            val c = f(a, b)
            cache = Some(c)
            c
          def isCancelled(): Boolean =
            futureA.isCancelled() || futureB.isCancelled()

          def cancel(mayInterruptIfRunning: Boolean): Boolean =
            futureA.cancel(mayInterruptIfRunning) | futureB.cancel(
              mayInterruptIfRunning
            )

        // UnitFuture(f(futureA.get, futureB.get))

// def sum(ints: IndexedSeq[Int])(using Par: Par[Int]): Par[Int] =
//   if ints.size <= 1 then Par.unit(ints.headOption.getOrElse(0))
//   else
//     val (l, r) = ints.splitAt(ints.size / 2)
//     Par.fork(sum(l)).map2(Par.fork(sum(r)))(_ + _)
