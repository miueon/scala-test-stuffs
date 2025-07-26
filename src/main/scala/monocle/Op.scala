package monocle

import cats.Contravariant
import cats.Functor
import cats.Id
import cats.arrow.Profunctor
import cats.data.Const
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*

/** Profunctor instance for Const - used for Review optics */
given [X]: Profunctor[[A, B] =>> Const[B, A]] with
  def dimap[A, B, C, D](fab: Const[B, A])(f: C => A)(g: B => D): Const[D, C] =
    Const(g(fab.getConst))

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
type Getter[S, A] = Optic[Function1, [x] =>> Const[A, x], S, A]
// Getter[S, A] = (A => Const[A, A]) => (S => Const[S, S])
// to(sa: S => A) = (p: (A => Const[A, A])) => p.dimap(sa)(fa => fa.contramap(sa))
// to elevate the sa to the p[a, f[a]] => p[s, f[s]]
//

def to[P[_, _]: Profunctor, F[_]: Contravariant, S, A](sa: S => A): Optic[P, F, S, A] =
  (p: P[A, F[A]]) => p.dimap(sa)(fa => fa.contramap(sa))

extension [S](s: S)
  def ^[A](g: Getter[S, A]): A =
    g(identity[A].andThen(Const.apply))(s).getConst

object Get:
  case class Person(name: String, age: Int, addr: Address)
  case class Address(street: String, city: String)
  val alice = Person("Alice", 30, Address("123 Main", "Anytown"))

  // --------- View with to + ^. ---------
  val _name: Getter[Person, String] = to((p: Person) => p.name)(using functionProfunctor)
  val _age: Getter[Person, Int] = to((p: Person) => p.age)(using functionProfunctor)
  val _addrStreet: Getter[Person, String] = to((p: Person) => p.addr.street)(using functionProfunctor)
  val _addrCity: Getter[Person, String] = to((p: Person) => p.addr.city)(using functionProfunctor)

  val name = alice ^ _name
  val addrStreet = alice ^ _addrStreet
  
  // Expanding alice ^ _name to show underlying function applications:
  // 
  // 1. _name is a Getter[Person, String] created by:
  //    _name = to((p: Person) => p.name)
  //    = (p: Function1[String, Const[String, String]]) => p.dimap((p: Person) => p.name)(fa => fa.contramap((p: Person) => p.name))
  //
  // 2. alice ^ _name calls the ^ operator:
  //    def ^[A](g: Getter[S, A]): A = g(identity[A].andThen(Const.apply))(s).getConst
  //
  // 3. Step by step expansion:
  val nameExpanded = {
    // Step 1: Create the profunctor input for the getter
    val profunctorInput: String => Const[String, String] = identity[String].andThen(Const.apply)
    // This is: (s: String) => Const(s)
    
    // Step 2: Apply the getter to the profunctor input
    val getterResult: Person => Const[String, Person] = _name(profunctorInput)
    // This applies dimap to transform String => Const[String, String] into Person => Const[String, Person]
    
    // Step 3: Apply the result function to alice
    val constResult: Const[String, Person] = getterResult(alice)
    // This extracts alice.name and wraps it in Const
    
    // Step 4: Extract the value from Const
    val finalResult: String = constResult.getConst
    // This unwraps the String from Const[String, String]
    
    finalResult
  }
  
  // Manual expansion showing each transformation:
  val nameManual = {
    // _name = to((p: Person) => p.name) creates:
    // (p: String => Const[String, String]) => p.dimap((person: Person) => person.name)(fa => fa.contramap((person: Person) => person.name))
    
    // When we call alice ^ _name:
    // 1. profunctorInput = (s: String) => Const(s)
    val profunctorInput = (s: String) => Const(s)
    
    // 2. Apply dimap transformation:
    // p.dimap(person => person.name)(fa => fa.contramap(person => person.name))
    // Since F[_] = Const[String, _] is Contravariant, contramap ignores the function
    val transformed = profunctorInput.compose((person: Person) => person.name)
    // This becomes: (person: Person) => Const(person.name)
    
    // 3. Apply to alice and extract
    transformed(alice).getConst
  }
  
  // Verification that all approaches yield the same result:
  println(s"Original:     ${name}")
  println(s"Expanded:     ${nameExpanded}")
  println(s"Manual:       ${nameManual}")
  println(s"All equal:    ${name == nameExpanded && nameExpanded == nameManual}")

end Get

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

type Setter[S, T, A, B] = Optical[Function1, Function1, Id, S, T, A, B]
type Review[S, T, A, B] = Optical[[A, B] =>> Const[B, A], Function1, Id, S, T, A, B]
type Review_[S, A] = Review[S, S, A, A]

/** PPrism for sum types - proper profunctor encoding */
type PPrism[S, T, A, B] = Optical[Choice, Choice, Id, S, T, A, B]
type PPrism_[S, A] = PPrism[S, S, A, A]

/** Grate for distributed operations - proper profunctor encoding */
type Grate[S, T, A, B] = Optical[Closed, Closed, Id, S, T, A, B]
type Grate_[S, A] = Grate[S, S, A, A]


/** Constructor for Review optics */
def review[S, T, A, B](bt: B => T): Review[S, T, A, B] =
  (cab: Const[Id[B], A]) => (_: S) => bt(cab.getConst)

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
    r(Const(a))(null.asInstanceOf[S])

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

// --------- View with to + ^. ---------
val _name: Getter[Person, String] = to((p: Person) => p.name)(using functionProfunctor)
val _age: Getter[Person, Int] = to((p: Person) => p.age)(using functionProfunctor)

val name = alice ^ _name
val age = alice ^ _age

val modifyAge: Setter[Person, Person, Int, Int] = sets[Function1, Function1, Id, Person, Person, Int, Int] { f => p =>
  p.copy(age = f(p.age))
}(using functionProfunctor, functionProfunctor)

val bob = modifyAge(_ => 40)(alice)
val older = modifyAge(_ + 1)(alice)

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

object Test extends IOApp.Simple:
  def run: IO[Unit] =
    for
      _ <- IO.println("\n--- Getter Expansion Demo ---")
      _ <- IO(Get) // This will execute the println statements in Get object
      _ <- IO.println(s"Name: $name") >>
        IO.println(s"Age: $age")
      _ <- IO.println(s"Name: ${bob.name}") >>
        IO.println(s"Age: ${bob.age}")
      _ <- IO.println(s"Name: ${older.name}") >>
        IO.println(s"Age: ${older.age}")
      _ <- IO.println("\n--- Review Examples ---")
      _ <- IO.println(s"Charlie: ${charlie.name}, ${charlie.age}")
      _ <- IO.println(s"Young person: ${youngPerson.name}, ${youngPerson.age}")
      _ <- IO.println("\n--- PPrism Examples ---")
      _ <- IO.println(s"Extract from Right(42): $extractedInt")
      _ <- IO.println(s"Extract from Left(error): $noInt")
      _ <- IO.println("\n--- Grate Examples ---")
      _ <- IO.println(s"Original config: $sampleConfig")
      _ <- IO.println(s"Grate transformed config: $configWithPrefix")
    yield ()
end Test
