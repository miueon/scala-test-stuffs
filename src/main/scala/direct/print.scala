package direct.print

type Console = Console.type

type Print[A] = Console ?=> A
extension [A](print: Print[A])
  def prefix(first: Print[Unit]): Print[A] =
    Print {
      first
      print
    }

  def red: Print[A] =
    Print {
      Print.print(Console.RED)
      val result = print
      Print.print(Console.RESET)
      result
    }

object Print:
  inline def apply[A](inline body: Console ?=> A): Print[A] =
    body

  def print(msg: Any)(using c: Console): Unit =
    c.print(msg)

  def println(msg: Any)(using c: Console): Unit = 
    c.println(msg)

  def run[A](print: Print[A]): A =
    given c: Console = Console
    print

@main def go = 
  given c: Console = Console
  val message = Print.println("Hello")

  val red = Print.println("Am").prefix(Print.print("> ").red)

  // Print.run(message)
  // Print.run(red)
  
