package tl2

import io.circe.*
import io.circe.generic.semiauto
import scala.annotation.meta.field
import theworld.macro2.joinStr
import cats.InvariantMonoidal
import io.circe.Encoder.AsObject
import scala.compiletime.*
import cats.syntax.all.*
import scala.deriving.Mirror
// ref: https://blog.rockthejvm.com/practical-type-level-programming/#7-final-words
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

import Tuple.*
case class IsFMap[T <: Tuple, F[_]](value: Map[T, F])

given InvariantMonoidal[Encoder.AsObject] with
  def imap[A, B](fa: AsObject[A])(f: A => B)(g: B => A): AsObject[B] = fa.contramapObject(g)
  def product[A, B](fa: AsObject[A], fb: AsObject[B]): AsObject[(A, B)] = Encoder.AsObject.instance { case (a, b) =>
    val jsonA = fa.encodeObject(a)
    val jsonB = fb.encodeObject(b)
    JsonObject(jsonA.toList ++ jsonB.toList: _*)
  }
  def unit: AsObject[Unit] = Encoder.AsObject.instance(_ => JsonObject.empty)

inline def sequenceInvariantTuple[T <: Tuple, F[_]: cats.InvariantMonoidal](fs: Map[T, F]): F[T] =
  inline IsFMap(fs) match
    case _: IsFMap[EmptyTuple, F] => InvariantMonoidal[F].point(EmptyTuple)
    case ds: IsFMap[h *: t, F] =>
      InvariantMonoidal[F]
        .product(ds.value.head, sequenceInvariantTuple(ds.value.tail))
        .imap((h, t) => h *: t)(a => a.head -> a.tail)

inline def mkEncoder[A <: Product](using mirror: Mirror.ProductOf[A]): Encoder[A] =
  Encoder.instance[A] { value =>
    val encoders = summonAll[Map[mirror.MirroredElemTypes, Encoder.AsObject]]
    sequenceInvariantTuple(encoders).contramapObject[A](Tuple.fromProductTyped).encodeObject(value).toJson
  }

inline def mkDecoder[A <: Product](using mirror: Mirror.ProductOf[A]): Decoder[A] =
  sequenceInvariantTuple(summonAll[Map[mirror.MirroredElemTypes, Decoder]]).map(mirror.fromProduct)

inline def mkCodec[A <: Product](using mirror: Mirror.ProductOf[A]): Codec[A] =
  checkDuplicateFields[A]
  Codec.from(mkDecoder[A], mkEncoder[A])

trait Labelling[Lable <: String, ElemLabels <: Tuple]
// we want to extract the MirroredLabel and MirroredElemLabels from each Mirror instance and place them as the type argument to Labelling
// we want a type thing like:
//   Labelling["Email", ("primary", "secondary")] *:
// Labelling["Phone", ("number", "prefix")] *:
// Labelling["Address", ("country", "city")] *:
// EmptyTuple

class Typed[A]: // inspired by mirror, we can use this to compute type from value
// or say store type as value
  type Value = A

transparent inline def mkLabellings[T <: Tuple]: Typed[? <: Tuple] =
  inline erasedValue[T] match
    case EmptyTuple  => Typed[EmptyTuple]
    case _: (h *: t) =>
      // val headMirror = summonInline[Mirror.ProductOf[h]]
      // type headLebelling = Labelling[headMirror.MirroredLabel, headMirror.MirroredElemLabels]
      // val tailLabelling = mkLabellings[t]
      // Typed[headLebelling *: tailLabelling.Value]
      inline mkLabellings[t] match
        case tailLabellings =>
          inline summonInline[Mirror.Of[h]] match
            case headMirror =>
              type HeadLabelling = Labelling[headMirror.MirroredLabel, headMirror.MirroredElemLabels]
              // Typed[Prepend[HeadLabelling, tailLabellings.Value]]
              Typed[HeadLabelling *: tailLabellings.Value]

// For scala3 type hack, we need to use
// - inline to keep type info
// - transparent to keep type precisely
// - class Typed[A]{type Value = A}, Type Prepend[X, +Y <: Typle] <: Typle to compose Tuple type as we want
type Prepend[X, +Y <: Tuple] <: Tuple = X match
  case X => X *: Y

type ZipWithSource[L] <: Tuple = L match
  case Labelling[label, elemLabels] =>
    ZipWithConst[elemLabels, label]

type ZipWithConst[T <: Tuple, A] =
  T Map ([t] =>> (t, A))

type ZipAllWithSource[Labelling <: Tuple] = Labelling FlatMap ZipWithSource

import scala.compiletime.ops.boolean.* // 1
import scala.compiletime.ops.any.*

type FindLabel[Label, T <: Tuple] =
  T Filter ([ls] =>> HasLabel[Label, ls]) Map Second

type RemoveLabel[Label, T <: Tuple] =
  T Filter ([ls] =>> ![HasLabel[Label, ls]])

type HasLabel[Label, LS] <: Boolean = LS match
  case (l, s) => Label == l

type Second[T] = T match
  case (a, b) => b

type GroupByLabels[Labels <: Tuple] <: Tuple = Labels match
  case EmptyTuple => EmptyTuple
  case (label, source) *: t =>
    ((label, source *: FindLabel[label, t])) *: GroupByLabels[RemoveLabel[label, t]]

import scala.compiletime.ops.int.*

type OnlyDupolicates[Labels <: Tuple] = Labels Filter ([t] =>> Size[Second[t]] > 1)

type FindDuplicates[Labellings <: Tuple] =
  OnlyDupolicates[GroupByLabels[ZipAllWithSource[Labellings]]]

type Size[T] <: Int = T match
  case EmptyTuple => 0
  case x *: xs    => 1 + Size[xs]

import scala.compiletime.constValueTuple

type Stuff = ("a", "b", "c")
val tupleValue = constValueTuple[Stuff]

import scala.compiletime.error

import scala.compiletime.ops.string

type MkString[T <: Tuple] =
  Fold[Init[T], Last[T], [a, b] =>> a ++ ", " ++ b]

type ++[A, B] <: String = (A, B) match
  case (a, b) => string.+[a, b]

type RenderLabelWithSources[LabelWithSources] <: String = LabelWithSources match
  case (label, sources) => "- [" ++ label ++ "] from [" ++ MkString[sources] ++ "]"

type RenderLabelsWithSources[LabelsWithSources <: Tuple] =
  LabelsWithSources Map RenderLabelWithSources

type RenderError[LabelsWithSources <: Tuple] =
  "Duiplicate fields found:\n" ++ Fold[RenderLabelsWithSources[LabelsWithSources], "", [a, b] =>> a ++ "\n" ++ b]

inline def renderDuplicatesError[Labellings <: Tuple]: Unit =
  type Duplicates = FindDuplicates[Labellings]
  inline erasedValue[Duplicates] match
    case _: EmptyTuple => ()
    case _: (h *: t)   => error(constValue[RenderError[h *: t]])

inline def checkDuplicateFields[A](using mirror: Mirror.ProductOf[A]): Unit =
  inline mkLabellings[mirror.MirroredElemTypes] match
    case labels => renderDuplicatesError[labels.Value]

// val t = mkLabellings[(Email, Phone, Address)]

// val mirror = summon[Mirror.Of[User]]
// val t1 = mkLabellings[mirror.MirroredElemTypes]
// type Labels = ZipAllWithSource[t1.Value]
// type Res1 = FindLabel["primary", Labels]
// type Res2 = RemoveLabel["primary", Labels]
// type Res3 = FindDuplicates[Labels]
// type Strings = ("a", "b", "c")
// type Res4 = MkString[Strings]

// type Result = RenderError[Labels]
// @main
// def test =
//   val user = User(Email("123@123.com", None), Phone("123456", 1), Address("USA", "NY"))
//   val codec = mkCodec[User]
//   val json = codec(user)
//   println(json)
//   println(codec.decodeJson(json))
