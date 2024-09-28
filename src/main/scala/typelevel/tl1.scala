package tl1

import io.circe.*
import io.circe.generic.semiauto
import scala.annotation.meta.field
import theworld.macro2.joinStr
import cats.InvariantMonoidal
import io.circe.Encoder.AsObject

opaque type T = String

case class User(email: Email, phone: Phone, address: Address)

case class Email(primary: String, secondary: Option[String])

case class Phone(number: String, prefix: Int)

case class Address(country: String, city: String)

object Email:
  given Codec.AsObject[Email] = semiauto.deriveCodec[Email]

object Phone:
  given Codec.AsObject[Phone] = semiauto.deriveCodec[Phone]

object Address:
  given Codec.AsObject[Address] = semiauto.deriveCodec[Address]

import scala.compiletime.*

inline def tupleToJson(tuple: Tuple): List[JsonObject] =
  inline tuple match
    case EmptyTuple => Nil
    case tup: (h *: t) =>
      val encoder = summonInline[Encoder.AsObject[h]]

      val json = encoder.encodeObject(tup.head)

      json :: tupleToJson(tup.tail)

def concatObjects(jsons: List[JsonObject]): Json =
  Json.obj(jsons.flatMap(_.toList)*)

val user = User(Email("123@123.com", None), Phone("123456", 1), Address("USA", "NY"))
val list = tupleToJson(Tuple.fromProductTyped(user))
val encoder = Encoder.instance[User] { value =>
  val fields = Tuple.fromProductTyped(value)
  val jsons = tupleToJson(fields)

  concatObjects(jsons)
}
import scala.deriving.*

inline def size[T <: Tuple]: Int =
  inline erasedValue[T] match
    case EmptyTuple  => 0
    case _: (h *: t) => 1 + size[t]

trait Is[A]

inline def decodeTuple[T <: Tuple]: Decoder[T] =
  inline erasedValue[Is[T]] match
    case _: Is[EmptyTuple] => Decoder.const(EmptyTuple)
    case _: Is[h *: t] =>
      val decoder = summonInline[Decoder[h]]
      combineDecoders(decoder, decodeTuple[t])

import Tuple.*

case class IsFMap[T <: Tuple, F[_]](value: Map[T, F])

inline def decodeTupleM[T <: Tuple](decoders: Map[T, Decoder]): Decoder[T] =
  inline IsFMap(decoders) match
    case _: IsFMap[EmptyTuple, Decoder] => Decoder.const(EmptyTuple)
    case ds: IsFMap[h *: t, Decoder] =>
      combineDecoders(ds.value.head, decodeTupleM(ds.value.tail))

inline def makeDecoder[A <: Product](using mirror: Mirror.ProductOf[A]): Decoder[A] =
  val decoders = summonAll[Map[mirror.MirroredElemTypes, Decoder]]

  decodeTupleM(decoders).map(mirror.fromTuple)

def combineDecoders[H, T <: Tuple](dh: Decoder[H], dt: Decoder[T]): Decoder[H *: T] =
  dh.product(dt).map(_ *: _)

import cats.syntax.all.*

inline def sequenceTuple[T <: Tuple, F[_]: cats.Applicative](fs: Map[T, F]): F[T] =
  inline IsFMap(fs) match
    case _: IsFMap[EmptyTuple, F] => EmptyTuple.pure[F]
    case ds: IsFMap[h *: t, F] =>
      ds.value.head.map2(sequenceTuple(ds.value.tail))(_ *: _)

inline def sequenceTupleInvariant[T <: Tuple, F[_]: cats.Applicative](fs: Map[T, F]): F[T] =
  sequenceInvariantTuple(fs)

inline def decodeTupleS[T <: Tuple](decoders: Map[T, Decoder]): Decoder[T] =
  sequenceTuple(decoders)

inline def sequenceInvariantTuple[T <: Tuple, F[_]: cats.InvariantMonoidal](fs: Map[T, F]): F[T] =
  inline IsFMap(fs) match
    case _: IsFMap[EmptyTuple, F] => InvariantMonoidal[F].point(EmptyTuple)
    case ds: IsFMap[h *: t, F] =>
      InvariantMonoidal[F]
        .product(ds.value.head, sequenceInvariantTuple(ds.value.tail))
        .imap((h, t) => h *: t)(a => a.head -> a.tail)

val emptyEncoder: Encoder.AsObject[EmptyTuple] = Encoder.AsObject.instance(_ => JsonObject.empty)
def combineObjectEncoders[H, T <: Tuple](eh: Encoder.AsObject[H], et: Encoder.AsObject[T]): Encoder.AsObject[H *: T] =
  Encoder.AsObject.instance[H *: T] { case h *: t =>
    val jsonH = eh.encodeObject(h)
    val jsonT = et.encodeObject(t)
    JsonObject(jsonH.toList ++ jsonT.toList: _*)
  }
inline def encodeTuple[T <: Tuple](encoders: Map[T, Encoder.AsObject]): Encoder.AsObject[T] =
  inline IsFMap(encoders) match
    case _: IsFMap[EmptyTuple, Encoder.AsObject] => emptyEncoder
    case es: IsFMap[h *: t, Encoder.AsObject] =>
      combineObjectEncoders(es.value.head, encodeTuple(es.value.tail))

given InvariantMonoidal[Encoder.AsObject] with
  def imap[A, B](fa: AsObject[A])(f: A => B)(g: B => A): AsObject[B] = fa.contramapObject(g)
  def product[A, B](fa: AsObject[A], fb: AsObject[B]): AsObject[(A, B)] = Encoder.AsObject.instance { case (a, b) =>
    val jsonA = fa.encodeObject(a)
    val jsonB = fb.encodeObject(b)
    JsonObject(jsonA.toList ++ jsonB.toList: _*)
  }
  def unit: AsObject[Unit] = Encoder.AsObject.instance(_ => JsonObject.empty)

inline def encodeTupleS[T <: Tuple](encoders: Map[T, Encoder.AsObject]): Encoder.AsObject[T] =
  sequenceInvariantTuple(encoders)
inline def makeEncoder[A <: Product](using mirror: Mirror.ProductOf[A]): Encoder[A] =
  Encoder.instance[A] { value =>
    val encoders = summonAll[Map[mirror.MirroredElemTypes, Encoder.AsObject]]
    val encoderTuple = encodeTuple(encoders).contramapObject[A](Tuple.fromProductTyped)
    encoderTuple.encodeObject(value).toJson
  }

val json = encoder(user)
val mirror = summon[Mirror.Of[User]]

val decoder = decodeTuple[mirror.MirroredElemTypes].map(mirror.fromTuple)

val codec = Codec.from(decoder, encoder)

val r = decodeTuple[mirror.MirroredElemTypes].decodeJson(json)

inline def makeCodec[A <: Product](using mirror: Mirror.ProductOf[A]): Codec[A] =
  val encoder = Encoder.instance[A] { value =>
    val fields = Tuple.fromProductTyped(value)
    val jsons = tupleToJson(fields)
    concatObjects(jsons)
  }
  val decoder = decodeTuple[mirror.MirroredElemTypes].map(mirror.fromProduct)

  Codec.from(decoder, encoder)

@main
def test =
  val json = codec(user)
  val decodedUser = codec.decodeJson(json)
  println(decodedUser)
