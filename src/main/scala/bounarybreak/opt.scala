package boundarybreak
import scala.util.boundary, boundary.{Label, break}

object nullable:
  inline def apply[T](inline body: Label[Null] ?=> T): T | Null =
    boundary(body)

  extension [T](r: T | Null)
    transparent inline def ?(using Label[Null]): T =
      if r == null then break(null) else r.nn

object failable:
  inline def apply[E, T](
      inline body: Label[Either[E, Nothing]] ?=> Either[E, T]
  ): Either[E, T] =
    boundary(body)

  extension [E, T](r: Either[E, T])
    transparent inline def !(using Label[Either[E, Nothing]]): T =
      r match
        case r: Right[?, T] => r.value
        case e: Left[E, ?]  => break(e.asInstanceOf)

case class A(b: B | Null)
case class B(c: C | Null)
case class C(d: D | Null)
case class D(i: Int)

import nullable.*
import failable.*
@main
def test =
  val a: A = A(B(null))
  // val a: A = null
  val v = nullable:
    val i = a.b.?.c.?.d.?.i
    Some(i)

  println(v)

  val str = failable:
    val str: String = Right[String, String]("error").!
    val i = str.toIntOption.toRight("error msg").!
    Right(i)
  println(str)

