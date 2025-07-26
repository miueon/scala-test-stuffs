package mtc

sealed trait People
case class Person(name: String, address: String, age: Int) extends People
case class Couple(person1: Person, person2: Person) extends People
trait LowPrioImplicits:
  inline given [T]: Show[T] = Debug.print( deriveShow[T])

object Implicits extends LowPrioImplicits:
  given Show[String] with
    def show(t: String): String = t

  given Show[Int] with
    def show(t: Int): String = t.toString

import Implicits.given

def display(p: People)(using s: Show[People]) =
  println(s"Show: ${s.show(p)}")

def displayF(t: F)(using s: Show[F]) = 
  println(s"Show ${s.show(t)}")

enum F:
  case A

// @main
// def test =
//   val p1: People = Person("John", "zz", 22)
//   display(p1)
  // displayF(F.A)
