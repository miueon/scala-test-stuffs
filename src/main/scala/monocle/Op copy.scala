package monoclel

import cats.Contravariant
import cats.Functor
import cats.Id
import cats.arrow.Profunctor
import cats.data.Const
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*

/** PROFUNCTOR OPTICS ENCODING EXPLAINED
  *
  * This file demonstrates the profunctor encoding of optics, which is the theoretical foundation behind libraries like
  * Monocle. The key insight is that every optic can be represented as a function that transforms one profunctor into
  * another.
  *
  * The type signature: Optic s t a b = forall p. Profunctor p => p a b -> p s t
  *
  * This means: "Given any profunctor p and a way to transform 'a' to 'b', I can show you how to transform 's' to 't'"
  *
  * Different profunctor constraints give us different optics:
  *   - Strong: enables lens (focus on part of product type)
  *   - Choice: enables prism (focus on part of sum type)
  *   - Strong + Choice: enables optional (lens that can fail)
  *   - Closed: enables grate (distribute over function space)
  */

/** Tagged profunctor for Review optics */
case class Tagged[A, B](unTagged: B)

given Profunctor[Tagged] with
  def dimap[A, B, C, D](fab: Tagged[A, B])(f: C => A)(g: B => D): Tagged[C, D] =
    Tagged(g(fab.unTagged))

/** PROFUNCTOR DIMAP EXPLAINED
  *
  * dimap is the core operation of profunctors. It allows us to transform both the input and output of a profunctor. The
  * signature:
  *
  * dimap: (C => A) => (B => D) => P[A, B] => P[C, D]
  *
  * This says: "If I can convert C to A (contravariant in first position) and B to D (covariant in second position),
  * then I can lift a P[A,B] to P[C,D]"
  *
  * This is exactly what we need for optics composition and transformation.
  */

/** Choice profunctor constraint - represents profunctors that can handle sum types */
trait ChoiceP[P[_, _]] extends Profunctor[P]:
  def left[A, B, C](pab: P[A, B]): P[Either[A, C], Either[B, C]]
  def right[A, B, C](pab: P[A, B]): P[Either[C, A], Either[C, B]]

given ChoiceP[Function1] with
  def dimap[A, B, C, D](fab: A => B)(f: C => A)(g: B => D): C => D =
    c => g(fab(f(c)))
  def left[A, B, C](pab: A => B): Either[A, C] => Either[B, C] =
    _.fold(a => Left(pab(a)), c => Right(c))
  def right[A, B, C](pab: A => B): Either[C, A] => Either[C, B] =
    _.fold(c => Left(c), a => Right(pab(a)))

/** Closed profunctor constraint - represents profunctors that can handle function distribution */
trait ClosedP[P[_, _]] extends Profunctor[P]:
  def closed[A, B, X](pab: P[A, B]): P[X => A, X => B]

given ClosedP[Function1] with
  def dimap[A, B, C, D](fab: A => B)(f: C => A)(g: B => D): C => D =
    c => g(fab(f(c)))
  def closed[A, B, X](pab: A => B): (X => A) => (X => B) =
    xa => x => pab(xa(x))

/** Dedicated profunctor wrappers for type safety */
case class ChoiceArrow[A, B](run: A => B)
case class ClosedArrow[A, B](run: A => B)

given Profunctor[ChoiceArrow] with
  def dimap[A, B, C, D](fab: ChoiceArrow[A, B])(f: C => A)(g: B => D): ChoiceArrow[C, D] =
    ChoiceArrow(c => g(fab.run(f(c))))

given Profunctor[ClosedArrow] with
  def dimap[A, B, C, D](fab: ClosedArrow[A, B])(f: C => A)(g: B => D): ClosedArrow[C, D] =
    ClosedArrow(c => g(fab.run(f(c))))

/** Type aliases for optics */
type Choice[A, B] = ChoiceArrow[A, B]
type Closed[A, B] = ClosedArrow[A, B]

/** Minimal encoding of lens’s Settable */
trait Settable[F[_]] extends Functor[F]:
  def tainted[A](a: A): F[A] // wrap
  def untainted[A](fa: F[A]): A // unwrap

// Two canonical Settable functors used by setters & traversals
given Settable[Id] with
  def map[A, B](fa: A)(f: A => B): B = f(fa)
  def tainted[A](a: A): A = a
  def untainted[A](a: A): A = a

given [C]: Settable[[x] =>> Const[C, x]] with
  def map[A, B](fa: Const[C, A])(f: A => B): Const[C, B] = fa.retag
  def tainted[A](a: A): Const[C, A] = ??? // never used in setters
  def untainted[A](fa: Const[C, A]): A = ??? // never used either

given optionSettable: Settable[Option] with
  def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  def tainted[A](a: A): Option[A] = Some(a)
  def untainted[A](fa: Option[A]): A = fa.get

type Optic[P[_, _], F[_], S, A] = P[A, F[A]] => P[S, F[S]]

def to[P[_, _]: Profunctor, F[_]: Contravariant, S, A](sa: S => A): Optic[P, F, S, A] =
  (p: P[A, F[A]]) => p.dimap(sa)(fa => fa.contramap(sa))

given functionProfunctor: Profunctor[Function1] with
  def dimap[A, B, C, D](fab: A => B)(f: C => A)(g: B => D): C => D =
    c => g(fab(f(c)))

type Optical[P[_, _], Q[_, _], F[_], S, T, A, B] = P[A, F[B]] => Q[S, F[T]]

def sets[P[_, _]: Profunctor, Q[_, _]: Profunctor, F[_]: Settable, S, T, A, B](
    iso: P[A, B] => Q[S, T]
): Optical[P, Q, F, S, T, A, B] =
  (pafb: P[A, F[B]]) =>
    val pab = pafb.rmap { fb => summon[Settable[F]].untainted(fb) }
    val qst = iso(pab)
    qst.rmap { st => summon[Settable[F]].tainted(st) }

type Getter[S, A] = Optic[Function1, [x] =>> Const[A, x], S, A]
type Setter[S, T, A, B] = Optical[Function1, Function1, Id, S, T, A, B]
type Review[S, T, A, B] = Optical[Tagged, Function1, Id, S, T, A, B]
type Review_[S, A] = Review[S, S, A, A]

/** PPrism for sum types - proper profunctor encoding */
type PPrism[S, T, A, B] = Optical[Choice, Choice, Id, S, T, A, B]
type PPrism_[S, A] = PPrism[S, S, A, A]

/** Strong profunctor constraint - for profunctors that can handle product types */
trait Strong[P[_, _]] extends Profunctor[P]:
  def first[A, B, C](pab: P[A, B]): P[(A, C), (B, C)]
  def second[A, B, C](pab: P[A, B]): P[(C, A), (C, B)]

given Strong[Function1] with
  def dimap[A, B, C, D](fab: A => B)(f: C => A)(g: B => D): C => D =
    c => g(fab(f(c)))
  def first[A, B, C](pab: A => B): ((A, C)) => (B, C) =
    (ac: (A, C)) => (pab(ac._1), ac._2)
  def second[A, B, C](pab: A => B): ((C, A)) => (C, B) =
    (ca: (C, A)) => (ca._1, pab(ca._2))

/** PLens for product types - case class with getter and setter */
case class PLens_[S, A](get: S => A, set: A => S => S)

/** POptional combines Choice and Strong - case class implementation */
case class POptional_[S, A](preview: S => Option[A], set: A => S => S)

/** Constructor for POptional - like lens but can fail */
def optional[S, A](preview: S => Option[A])(setter: (S, A) => S): POptional_[S, A] =
  POptional_(preview, a => s => setter(s, a))

extension [S](s: S)
  def getOption[A](opt: POptional_[S, A]): Option[A] =
    opt.preview(s)

extension [S, A](opt: POptional_[S, A])
  def set(a: A)(s: S): S =
    opt.preview(s) match
      case Some(_) => opt.set(a)(s)
      case None    => s

  def modify(f: A => A)(s: S): S =
    opt.preview(s) match
      case Some(currentA) => opt.set(f(currentA))(s)
      case None           => s

/** PTraversal for operating on multiple elements */
type PTraversal[S, T, A, B] = [F[_]] => (F: cats.Applicative[F]) => (A => F[B]) => S => F[T]
type PTraversal_[S, A] = PTraversal[S, S, A, A]

/** Constructor for PTraversal - traverse and modify all elements */
def traversal[S, A](extract: S => List[A])(rebuild: (S, List[A]) => S): PTraversal_[S, A] =
  [F[_]] =>
    (F: cats.Applicative[F]) =>
      (f: A => F[A]) =>
        (s: S) =>
          val items = extract(s)
          import cats.syntax.all.*
          given cats.Applicative[F] = F
          items.traverse(f).map(newItems => rebuild(s, newItems))

extension [S](s: S)
  def modifyAll[A](trav: PTraversal_[S, A])(f: A => A): S =
    trav[cats.Id](cats.catsInstancesForId)(f)(s)

/** Grate for distributed operations - proper profunctor encoding */
type Grate[S, T, A, B] = Optical[Closed, Closed, Id, S, T, A, B]
type Grate_[S, A] = Grate[S, S, A, A]

/** Constructor for PLens optics */
def lens[S, A](getter: S => A)(setter: (S, A) => S): PLens_[S, A] =
  PLens_(getter, a => s => setter(s, a))

extension [S](s: S)
  def view[A](l: PLens_[S, A]): A =
    l.get(s)

extension [S, A](l: PLens_[S, A])
  def set(a: A)(s: S): S =
    l.set(a)(s)

  def modify(f: A => A)(s: S): S =
    l.set(f(l.get(s)))(s)

/** Constructor for Review optics */
def review[S, T, A, B](bt: B => T): Review[S, T, A, B] =
  (tab: Tagged[A, Id[B]]) => (_: S) => bt(tab.unTagged)

/** Constructor for PPrism optics */
def prism[S, T, A, B](preview: S => Either[T, A])(review: B => T): PPrism[S, T, A, B] =
  (choice: Choice[A, Id[B]]) =>
    ChoiceArrow(s =>
      preview(s) match
        case Left(t)  => t
        case Right(a) => review(choice.run(a))
    )

/** Constructor for Grate optics */
def grate[S, T, A, B](coalgebra: ((S => A) => B) => T): Grate[S, T, A, B] =
  (closed: Closed[A, Id[B]]) => ClosedArrow(s => coalgebra(sa => closed.run(sa(s))))

/** Usage operator for Review - construct from value */
extension [A](a: A)
  def #>[S](r: Review_[S, A]): S =
    r(Tagged(a))(null.asInstanceOf[S])

/** Preview operation for PPrism using Const functor */
def previewPrism[S, A](p: PPrism_[S, A])(s: S): Option[A] =
  p(ChoiceArrow(identity[A])).run(s) match
    case Right(a) => Some(a.asInstanceOf[A])
    case Left(_)  => None
    case a        => Some(a.asInstanceOf[A])

/** Usage operators for PPrism */
extension [S](s: S) def preview[A](p: PPrism_[S, A]): Option[A] = previewPrism(p)(s)

/** Usage operators for Grate */
extension [S](s: S)
  def transformWith[A](g: Grate_[S, A])(f: (S => A) => A): S =
    val arrow = ClosedArrow((a: A) => f((_: S) => a))
    val transformed = g(arrow)
    transformed.run(s)

case class Person(name: String, age: Int)
val alice = Person("Alice", 30)

// --------- View with PLens ---------
val nameLens: PLens_[Person, String] = lens((p: Person) => p.name)((p: Person, n: String) => p.copy(name = n))
val ageLens: PLens_[Person, Int] = lens((p: Person) => p.age)((p: Person, a: Int) => p.copy(age = a))

val name = alice.view(nameLens)
val age = alice.view(ageLens)

val bob = ageLens.set(40)(alice)
val older = ageLens.modify(_ + 1)(alice)

// --------- POptional examples ---------
case class Container(items: List[Int])
val containerOpt: POptional_[Container, Int] =
  optional((c: Container) => c.items.headOption)((c: Container, i: Int) => c.copy(items = i :: c.items.tail))

val emptyContainer = Container(Nil)
val fullContainer = Container(List(1, 2, 3))

val headFromEmpty = emptyContainer.getOption(containerOpt) // None
val headFromFull = fullContainer.getOption(containerOpt) // Some(1)

// --------- PTraversal examples ---------
val listTraversal: PTraversal_[List[Int], Int] = traversal[List[Int], Int](
  identity
)((_, newItems) => newItems)

val numbers = List(1, 2, 3, 4, 5)
val doubled = numbers.modifyAll(listTraversal)(_ * 2) // List(2, 4, 6, 8, 10)

// --------- Review examples ---------
val _personFromName: Review_[Person, String] = review(name => Person(name, 0))
val _personFromAge: Review_[Person, Int] = review(age => Person("Unknown", age))

val charlie = "Charlie" #> _personFromName
val youngPerson = 25 #> _personFromAge

// --------- PPrism examples with Either ---------
val leftString: Either[String, Int] = Left("error")
val rightInt: Either[String, Int] = Right(42)

val _Right: PPrism_[Either[String, Int], Int] = prism[Either[String, Int], Either[String, Int], Int, Int] {
  case Right(i) => Right(i)
  case Left(e)  => Left(Left(e))
}((b: Int) => Right(b))

val extractedInt = rightInt.preview(_Right)
val noInt = leftString.preview(_Right)

// --------- Grate example with function transformation ---------
case class Config(host: String, port: Int)
def envLookup(key: String): String = s"$key-value"

val _configGrate: Grate_[Config, String] = grate { f =>
  Config(f(_.host), 8080) // Fixed port for demonstration
}

val sampleConfig = Config("localhost", 8080)
val configWithPrefix = sampleConfig.transformWith(_configGrate)(f => s"prefix-${f(sampleConfig)}")

object Sample extends IOApp.Simple:
  def run: IO[Unit] =
    for
      _ <- IO.println("=== PLens Examples ===")
      _ <- IO.println(s"Alice: $name, $age")
      _ <- IO.println(s"Bob: ${bob.name}, ${bob.age}")
      _ <- IO.println(s"Older: ${older.name}, ${older.age}")
      _ <- IO.println("\n=== POptional Examples ===")
      _ <- IO.println(s"Head from empty: $headFromEmpty")
      _ <- IO.println(s"Head from full: $headFromFull")
      _ <- IO.println("\n=== PTraversal Examples ===")
      _ <- IO.println(s"Original: $numbers")
      _ <- IO.println(s"Doubled: $doubled")
      _ <- IO.println("\n=== Review Examples ===")
      _ <- IO.println(s"Charlie: ${charlie.name}, ${charlie.age}")
      _ <- IO.println(s"Young person: ${youngPerson.name}, ${youngPerson.age}")
      _ <- IO.println("\n=== PPrism Examples ===")
      _ <- IO.println(s"Extract from Right(42): $extractedInt")
      _ <- IO.println(s"Extract from Left(error): $noInt")
      _ <- IO.println("\n=== Grate Examples ===")
      _ <- IO.println(s"Original config: $sampleConfig")
      _ <- IO.println(s"Grate transformed config: $configWithPrefix")
    yield ()
end Sample
