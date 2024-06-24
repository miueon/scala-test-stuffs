package temp

import cats.effect.IOApp
import cats.effect.IO
import cats.Show
import cats.derived.*

enum Predicate derives Show:
  case P1
  case P2
  case Ptrue
  case Pfalse

enum Instruction derives Show:
  case LDA(p: Predicate, i: Int)
  case LDB(p: Predicate, i: Int)
  case SET_gt(p1: Predicate, p2: Predicate)
  case CLR(p1: Predicate, p2: Predicate)
  case SET(p1: Predicate, p2: Predicate)
  case STA(p: Predicate, i: Int)
  case STB(p: Predicate, i: Int)
  case HALT(p: Predicate)

import Instruction.*
import Predicate.*
def ex2InstrStream(n: Int): List[Instruction] = {
  (1 until n).flatMap { i =>
    List(SET(Ptrue, P2)) ++
      (0 until (n - i - 1)).flatMap { j =>
        List(
          LDA(Ptrue, j),
          LDB(Ptrue, j + 1),
          SET_gt(Ptrue, P1),
          STA(P1, j + 1),
          STB(P1, j),
          CLR(P1, P2)
        )
      } ++ List(HALT(P2))
  }.toList
}

object ex3 extends IOApp.Simple:

  def run: IO[Unit] = 
    for 
      instrs <- IO(ex2InstrStream(5))
      _ <- IO.println(instrs.mkString("\n"))
    yield ()
