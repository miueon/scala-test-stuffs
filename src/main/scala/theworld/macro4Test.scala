package theworld.macro4
case class A(x: Int)
@main
def test = 
  var x = 0
  val ref = Ref(x)

  var t = A(10)
  val refT = Ref(t)

  var f: Float = 1.0
  val ft = Ref(f)

  ref := 10

  println(x)

  val failed = 0

  // val refF = Ref(failed) // Reassignment to val failed



