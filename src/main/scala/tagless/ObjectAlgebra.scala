package tagless.objectalg

trait ExpAlg[T]:
  def lit(n: Int): T
  def add(x: T, y: T): T

trait Eval:
  def eval(): Int

class EvalExp extends ExpAlg[Eval]:
  def lit(n: Int): Eval =
    return new Eval:
      def eval(): Int = n

  def add(x: Eval, y: Eval): Eval =
    return new Eval:
      def eval(): Int = x.eval() + y.eval()

def e1[T](f: ExpAlg[T]): T =
  f.add(
    f.lit(1),
    f.add(
      f.lit(2),
      f.lit(3)
    )
  )

trait MulAlg[T] extends ExpAlg[T]:
  def mul(x: T, y: T): T

def e2[T](f: MulAlg[T]): T =
  f.mul(
    f.lit(2),
    f.add(
      f.lit(20),
      f.lit(1)
    )
  )

class EvalMul extends EvalExp, MulAlg[Eval]:
  def mul(x: Eval, y: Eval): Eval =
    new Eval():
      def eval(): Int =
        return x.eval() * y.eval()

trait View:
  def view(): String

class ViewExp extends ExpAlg[View]:
  def lit(n: Int): View = 
    new View:
      def view(): String = 
        return n.toString()

  def add(x: View, y: View): View = 
    new View:
      def view(): String = 
        s"(${x.view()} + ${y.view()})"

class ViewMul extends ViewExp, MulAlg[View]:
  def mul(x: View, y: View): View = 
    new View:
      def view(): String = 
        s"(${x.view()} * ${y.view()})"

@main
def test() =
  val v1 = e1(EvalExp()).eval()
  val v2 = e2(EvalMul()).eval()

  println(v1)
  println(v2)

  val v1v = e1(ViewExp()).view()
  val v2v = e2(ViewMul()).view()

  println(v1v)
  println(v2v)






