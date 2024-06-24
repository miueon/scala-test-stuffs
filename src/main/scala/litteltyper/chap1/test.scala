package lt.chap1

import cats.data.EitherT
import cats.data.StateT

abstract class B extends reflect.Selectable

val foo = new B:
  val x = 3
  val y = "hi"

@main
def test =
  println(foo.x)

  val t: "test" = "test"

  val b: "test" = "test"

  println(t == b)
  println(t eq b)

  val a = "a"

  val aa = "a"

  println(a == aa)
  println(a eq aa)

  println(t.isInstanceOf[String])
// import cats.data.*
// def checkState: EitherT[StateT[List, Int, ?], Exception, String] =
//   for
//     currentState <- EitherT.liftF(StateT.get[List, Int])
//     result <-
//       if currentState > 10 then
//         EitherT.liftT[StateT[List, Int, ?], String](new Exception)
//       else
//         EitherT.rightT[StateT[List, Int, ?], Exception](
//           s"current state is $currentState"
//         )
//   yield result
