package parseLine

import scala.util.Try

def parseLine(line: String): Option[Int] =
  val parts = line.split(" ").toList
  for
    case List("Status:", "0", _, "Result:", result) <- Some(parts)
    n <- Try(result.toInt).toOption
    _ <- Option.when(n % 2 == 0)(())
  yield n

@main
def test =
  println(parseLine("Status: 0 | Result: 42"))
