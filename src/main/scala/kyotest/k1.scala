package k1

import kyo.*
import scala.util.control.NoStackTrace

val a: Int < Any = 1

val b: Int < IO = a

def example(v: Int < (IO & Abort[Exception])) =
  v.map(_ + 1)

def example3 = example(41)

enum SelfDefinedError extends NoStackTrace:
  case Error1

object K1 extends KyoApp:
  import SelfDefinedError.*
  run {
    for
      _ <- Console.println("Hello, world!")
      currentTime <- Clock.now
      _ <- Abort.catching(throw SelfDefinedError.Error1)
      
    yield "example"
  }