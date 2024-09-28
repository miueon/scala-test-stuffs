package theworld.macro4
case class A(x: Int)
case class B(var y: Int)
@main
def test = 
  var x = 0
  val ref = Ref(x)

  var t = A(10)
  val refT = Ref(t)

  var f: Float = 1.0
  val ft = Ref(f)

  ref := 10

  val y = B(42)

  val refY = Ref(y.y)

  println(ref)

  val failed = 0

  // val refF = Ref(failed) // Reassignment to val failed



