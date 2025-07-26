package guess

import cats.kernel.Eq
import cats.kernel.Order
import cats.kernel.Eq.catsKernelInstancesForInt
import cats.derived.semiauto
import cats.effect.IOApp
import cats.effect.IO
import cats.syntax.all.*

object SmallInt:
  opaque type SmallInt = Int

  def apply(v: Int): Option[SmallInt] =
    Option.when(v >= 1 && v <= 100)(v)

  def fromInt(v: Int) = apply(v)

  def unsafeFromInt(v: Int) = apply(v).getOrElse(
    throw new IllegalArgumentException("Invalid SmallInt value")
  )

  extension (si: SmallInt) def value: Int = si

  def unapply(si: SmallInt): Int = si.value

type SmallInt = SmallInt.SmallInt

given eqSmallInt: Eq[SmallInt] = Eq.by(_.value)
given ordSmallInt: Order[SmallInt] = Order.by(_.value)

object Main extends IOApp.Simple:
  def gameLoop(secret: SmallInt): IO[Unit] = 
    for
      _ <- IO.println("Pls input your guess")
      input <- IO.readLine

    yield ???
    ???

  def processGuess(input: String, secret: SmallInt): IO[Unit] = 
    val guessOpt = input.toIntOption.flatMap(SmallInt.apply)

    guessOpt match
      case None => 
        cats.effect.std.Console[IO].println(s"Guess must be a number within the range 1-100, not '$input'") >>
        gameLoop(secret)

      case Some(guess) =>
        guess.compare(secret) match
          case -1 => 
    ???

  def run: IO[Unit] = ???
