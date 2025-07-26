package maxsubsum

def subArraySum(arr: List[Int]): Int = 
  arr.foldRight((0, 0)) { (x, acc) =>
    val (maxEndingHere, maxSoFar) = acc
    val newMaxEndingHere = math.max(x, maxEndingHere + x)
    val newMaxSoFar = math.max(maxSoFar, newMaxEndingHere)
    (newMaxEndingHere, newMaxSoFar)
  }._2

@main
def test = 
  val a = subArraySum(List(5, 4, -1, 7, 8)) // 6
  println(a)