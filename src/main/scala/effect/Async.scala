package effect

import par.actor.Par

enum Async[A]:
  case Return(a: A)
  case Suspend(resume: Par[A])
  case FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

  def flatMap[B](f: A => Async[B]): Async[B] =
    FlatMap(this, f)

  def map[B](f: A => B): Async[B] =
    flatMap(a => Return(f(a)))

  @annotation.tailrec
  final def step: Async[A] = this match
    case FlatMap(FlatMap(x, f), g) => x.flatMap(a => f(a).flatMap(g)).step
    case FlatMap(Return(x), f) => f(x).step
    case _ => this

  def run: Par[A] = step match
    case Return(a) => Par.unit(a)
    case Suspend(resume) => resume
    case FlatMap(x, f) => x match
      case Suspend(resume) => resume.flatMap(a => f(a).run)
      case _ => sys.error("Impossible, since step eliminates these cases")
