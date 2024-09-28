package stateex

import cats.data.State

type CalcState[A] = State[List[Int], A]

def evalOne(sym: String): CalcState[Int] =
  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: t =>
        val ans = func(a, b)
        (ans :: t, ans)
      case _ =>
        sys.error("Fail")
    }

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }
  sym match
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num.toInt)
end evalOne

val program = 
  for
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  yield ans

@main
def test =
  println(program.run(Nil).value)

