package localeffect

import scala.reflect.ClassTag

opaque type ST[S, A] = () => A
object ST:
  extension [S, A](self: ST[S, A])
    def map[B](f: A => B): ST[S, B] =
      () => f(self())

    def flatMap[B](f: A => ST[S, B]): ST[S, B] =
      f(self())

  def apply[S, A](a: => A): ST[S, A] =
    lazy val memo = a
    () => memo

  def lift[S, A](f: () => A): ST[S, A] = f

  def run[A](st: [s] => () => ST[s, A]): A =
    val su = st[Unit]()
    su()

// opaque type ST[S, A] = S => (A, S)

// object ST:
//   extension [S, A](self: ST[S, A])
//     def map[B](f: A => B): ST[S, B] =
//       s =>
//         val (a, s1) = self(s)
//         (f(a), s1)

//     def flatMap[B](f: A => ST[S, B]): ST[S, B] =
//       s =>
//         val (a, s1) = self(s)
//         f(a)(s1)

//   def apply[S, A](a: => A): ST[S, A] =
//     lazy val memo = a
//     s => (memo, s)

//   def lift[S, A](f: S => (A, S)): ST[S, A] = f

//   def run[A](st: [s] => () => ST[s, A]): A =
//     val su = st[Unit]()
//     su(())(0)

// wrap the result of read and write with ST so the all the way down to ST.run if we want the operation result
final class STRef[S, A] private (private var cell: A):
  def read: ST[S, A] = ST(cell)

  def write(a: => A): ST[S, Unit] = ST.lift[S, Unit]:
    // s =>
    //   cell = a
    //   ((), s)
    () => cell = a

object STRef:
  def apply[S, A](a: A): ST[S, STRef[S, A]] =
    ST(new STRef[S, A](a))

final class STArray[S, A] private (private var value: Array[A]):
  def size: ST[S, Int] = ST(value.size)

  def write(i: Int, a: A): ST[S, Unit] =
    ST.lift[S, Unit] { () =>
      value(i) = a
      ()
    }

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  def fill(xs: Map[Int, A]): ST[S, Unit] =
    xs.foldRight(ST[S, Unit](())) { case ((k, v), _) =>
      write(k, v)
    }

  def swap(i: Int, j: Int): ST[S, Unit] = 
    for
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    yield ()

object STArray:
  def apply[S, A: ClassTag](
      sz: Int,
      v: A
  ): ST[S, STArray[S, A]] =
    ST(new STArray[S, A](Array.fill(sz)(v)))

  def fromList[S, A: ClassTag](xs: List[A]): ST[S, STArray[S, A]] = 
    ST(new STArray(xs.toArray))


