package compiletime

import scala.compiletime.summonFrom
trait A; trait B

inline def trySummonFrom(label: String, expected: Int): Unit = 
  val actual = summonFrom {
    case given A => 1
    case given B => 2
    case _ => 0
  }

  printf("%-9s trySummonFrom(): %d =?= %d\n", label, expected, actual)

def tryNone = trySummonFrom("tryNone:", 0)

def tryA = 
  given A with {}
  trySummonFrom("tryA:", 1)

def tryB = 
  given B with {
  
  }

  trySummonFrom("tryB:", 2)


def tryAB = 
  given A with {}
  given B with {}

  trySummonFrom("tryAB: ", 1)

@main
def test = 
  tryNone
  tryA
  tryB
  tryAB