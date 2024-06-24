package tagless.finallyt

trait ExpAlg[T]:
  def lit(n: Int): T
  def add(x: T, y: T): T

def e1[T](using f: ExpAlg[T]): T =
  f.add(
    f.lit(1),
    f.add(
      f.lit(2),
      f.lit(3)
    )
  )

case class Eval(eval: Int)

given EvalAlgIns: ExpAlg[Eval] with
  def lit(n: Int): Eval = Eval(n)
  def add(x: Eval, y: Eval): Eval = Eval(x.eval + y.eval)

trait MulAlg[T](using ExpAlg[T]):
  def mul(x: T, y: T): T

def e2[T: ExpAlg](using f: MulAlg[T]) =
  val t = summon[ExpAlg[T]]
  f.mul(
    t.lit(2),
    t.add(
      t.lit(20),
      t.lit(1)
    )
  )

given MulAlgIns: MulAlg[Eval] with
  def mul(x: Eval, y: Eval): Eval = Eval(x.eval * y.eval)

case class View(view: String)

given ExpAlgView: ExpAlg[View] with
  def add(x: View, y: View): View = View(s"(${x.view} + ${y.view})")

  def lit(n: Int): View = View(n.toString())

given MulAlgView: MulAlg[View] with
  def mul(x: View, y: View): View =
    View(s"(${x.view} * ${y.view})")

@main
def main() =
  val v1 = e2[Eval].eval

  val v1v = e2[View].view
  println(v1)
  println(v1v)
  