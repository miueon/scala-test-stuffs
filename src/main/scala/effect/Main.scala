package effect
import cats.syntax.all.*
import java.util.concurrent.Executors
import gears.async.*
import gears.async.default.given
case class Player(name: String, score: Int)

def winnerMsg(p: Option[Player]): String = p
  .map { case Player(name, _) =>
    s"$name is the winner!"
  }
  .getOrElse("It's a draw")

def winner(p1: Player, p2: Player): Option[Player] =
  if p1.score > p2.score then Some(p1)
  else if p1.score < p2.score then Some(p2)
  else None

def contest(p1: Player, p2: Player): IO[Unit] =
  PrintLine(winnerMsg(winner(p1, p2)))
object Main:
  def main(args: Array[String]): Unit =
    // contest(Player("Alice", 60), Player("Bob", 50)).unsafeRun
    val p = PrintLine("Still going...").foreverM
    Async.blocking:
      p.unsafeRunSync



