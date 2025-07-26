package privatethis


class MyClass:
  private var counter: Int = 0

  def increment() = counter += 1

  def getCounter(other: MyClass) = other.counter

class PrivateThis:
  private[this] var counter = 0

  def getCounter(other: PrivateThis) = other.counter
class Holder[+T](init: Option[T]):
  private var value = init

  def getValue = value
  def makeEmpty = value = None


@main
def test =
  val m1 = MyClass()

  println(MyClass().getCounter(MyClass()))

  println(PrivateThis().getCounter(PrivateThis())) // error: counter is private[this] and cannot be accessed from other instances
