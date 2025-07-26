package testgemini
import cats.effect.{IO, IOApp}
import cats.effect.std.Console
import cats.syntax.all.*
import cats.syntax.traverse.*
import cats.syntax.applicative.*
import scala.collection.immutable.List as IList

object EightQueensWithCats extends IOApp.Simple:

  case class Position(row: Int, col: Int)

  def isSafe(position: Position, placedQueens: IList[Position]): Boolean =
    placedQueens.forall { existingQueen =>
      position.col != existingQueen.col &&
      position.row != existingQueen.row &&
      Math.abs(position.row - existingQueen.row) != Math.abs(position.col - existingQueen.col)
    }

  def solveNQueens(n: Int): IO[IList[IList[Position]]] = IO {
    def placeQueen(row: Int, placedQueens: IList[Position]): IList[IList[Position]] =
      if row == n then IList(placedQueens)
      else
        (0 until n).toList
          .flatMap { col =>
            val newPosition = Position(row, col)
            if isSafe(newPosition, placedQueens) then placeQueen(row + 1, newPosition :: placedQueens)
            else IList.empty
          }
          .to(IList) // Convert to immutable List after flatMap
    placeQueen(0, IList.empty)
  }

  def printBoard(solution: IList[Position], n: Int): IO[Unit] =
    (0 until n).toList.traverse_ { row =>
      (0 until n).toList.traverse_ { col =>
        if solution.contains(Position(row, col)) then Console[IO].print("Q ")
        else Console[IO].print(". ")
      } *> Console[IO].println("") // New line after each row
    }

  def printBoardWithCats(solution: IList[Position], n: Int, catPositions: IList[Position]): IO[Unit] =
    (0 until n).toList.traverse_ { row =>
      (0 until n).toList.traverse_ { col =>
        if solution.contains(Position(row, col)) then Console[IO].print("Q ")
        else if catPositions.contains(Position(row, col)) then Console[IO].print("C ")
        else Console[IO].print(". ")
      } *> Console[IO].println("")
    }

  val run: IO[Unit] =
    val n = 8
    val catPositions = IList(Position(0, 1), Position(7, 6))

    for
      solutions <- solveNQueens(n)
      _ <- Console[IO].println(s"Found ${solutions.length} solutions for $n-Queens:")
      _ <- Console[IO].println("Example of a solution with cats:")
      _ <- printBoardWithCats(solutions.head, n, catPositions)
      _ <- Console[IO].println("\nFirst Solution (without cats):")
      _ <- printBoard(solutions.head, n)
    // Print all solutions (uncomment to use)
    /*
      _ <- solutions.zipWithIndex.traverse_ { (sol, index) =>
        Console[IO].println(s"\nSolution ${index + 1}:") *> printBoard(sol, n)
      }
     */
    yield ()
end EightQueensWithCats
