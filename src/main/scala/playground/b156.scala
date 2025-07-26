package b156

import scala.io.StdIn.*

@main
def test =
  // val (h, w) = readLine.split(" ").map(_.toInt) match
  //   case Array(h, w) => (h, w)
  // val (l, r) = readLine.split(" ").map(_.toInt) match
  //   case Array(l, r) => (l, r)

  // def readInputs(l: Int): List[List[Char]] =
  //   if l == 0 then Nil
  //   else
  //     val inputs = readLine.map(_.toChar).toList
  //     inputs :: readInputs(l - 1)

  // val inputs = readInputs(h)

  def lineerize(inputs: List[List[Char]], i: Int, j: Int, h: Int, w: Int): List[Char] =
    if i * 2 >= h || j * 2 >= w then Nil
    else
      val line1 = inputs.lift(i).map(it => it.drop(j).take(it.size - 2 * j)).getOrElse(Nil)
      val line2 = inputs.drop(i + 1).take(h - 2 * (i + 1)).map(it => it.lift(w - j - 1)).filter(_.isDefined).map(_.get)
      val line3 = if i != (h-i -1) then inputs.lift(h - i - 1).map(it => it.drop(j).take(it.size - 2 * j)).map(_.reverse).getOrElse(Nil) else Nil
      val line4 = inputs.drop(i + 1).take(h - 2 * (i + 1)).map(it => it.lift(j)).filter(_.isDefined).map(_.get).reverse
      line1 ++ line2 ++ line3 ++ line4 ++ lineerize(inputs, i + 1, j + 1, h, w)

  // paiza
// aizap
// pazia

  val testInput = List(
    List('p', 'a', 'i', 'z', 'a'),
    List('a', 'i', 'z', 'a', 'p'),
    List('p', 'a', 'z', 'i', 'a')
  )

  val (l, r) = (6, 10)
  val linearized = lineerize(testInput, 0, 0, 3, 5)

  println(linearized.mkString)

  println(linearized.mkString.drop(l - 1).take(r - l + 1))
end test
