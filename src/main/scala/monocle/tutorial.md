# Monocle Usage in This Project

This tutorial provides an overview of how [Monocle](https://github.com/julien-truffaut/Monocle) is used in this project to handle complex data structures with a focus on immutability and type safety. Monocle is a powerful optics library for Scala that simplifies the process of accessing and modifying nested case classes and other data structures.

## Core Concepts

We utilize several key features of Monocle to manage our application's state and data transformations effectively.

### `Iso` for Newtypes

One of the primary uses of Monocle in this project is to create zero-cost newtypes using `Iso`. An `Iso` is a type of optic that defines an isomorphism between two types, `A` and `B`. This is perfect for creating type-safe wrappers around primitive types without incurring any runtime overhead.

In `modules/domain/shared/src/main/scala/trading/Newtype.scala`, we define a `Newtype` abstract class that uses an `Iso` to wrap and unwrap a value, providing strong type guarantees.

```scala
import monocle.Iso

// ...

abstract class Newtype[A](
    // ... typeclass instances
):
  opaque type Type = A

  inline def apply(a: A): Type = a

  extension (t: Type) inline def value: A = t

  given Wrapper[A, Type] with
    def iso: Iso[A, Type] =
      Iso[A, Type](apply(_))(_.value)

  // ... derived typeclass instances
```

This pattern allows us to create distinct types for different kinds of `UUID`s or numerical values, preventing accidental mixing of, for example, a `UserId` with a `ProductId` even though they are both `UUID`s under the hood.

### `Focus` for Field Access

The `Focus` keyword from Monocle is used to generate lenses, which provide a type-safe way to access and modify fields within a case class. This is particularly useful for updating nested data structures without writing a lot of boilerplate code.

In `modules/domain/shared/src/main/scala/trading/state/TradeState.scala`, we use `Focus` to create lenses for the `TradeState` and `Prices` case classes:

```scala
import monocle.Focus

final case class TradeState(
    status: TradingStatus,
    prices: Map[Symbol, Prices]
)

object TradeState:
  val _Status = Focus[TradeState](_.status)
  val _Prices = Focus[TradeState](_.prices)

// ...

final case class Prices(
    ask: Prices.Ask,
    bid: Prices.Bid,
    high: Price,
    low: Price
)

object Prices:
  val _Ask  = Focus[Prices](_.ask)
  val _Bid  = Focus[Prices](_.bid)
  val _High = Focus[Prices](_.high)
  val _Low  = Focus[Prices](_.low)
```

These generated lenses (`_Status`, `_Prices`, etc.) can then be used to perform immutable updates on the `TradeState`.

### `Optional`, `At`, and `Index` for Maps

When dealing with `Map`s within our state, we often need to access or modify values that may or may not exist. Monocle's `Optional` optic is perfect for this. We combine it with `At` and `Index` to work with `Map`s in a safe and functional way.

*   `At.atMap` provides an `Optional` that focuses on a `Option[Value]` for a given key in a `Map`. This is useful for inserting, updating, or deleting values.
*   `Index.mapIndex` provides an `Optional` that focuses on a `Value` for a given key, but only if the key already exists in the `Map`.

Here is how we use them in `TradeState.scala` to manipulate the `prices` map:

```scala
import monocle.function.{ At, Index }
import monocle.Optional

object TradeState:
  // ... lenses from Focus

  object __Prices:
    def at(s: Symbol): Optional[TradeState, Option[Prices]] =
      _Prices.andThen(At.atMap[Symbol, Prices].at(s))

    def index(s: Symbol): Optional[TradeState, Prices] =
      _Prices.andThen(Index.mapIndex[Symbol, Prices].index(s))
```

These optics are then composed to modify the nested state in a clean and declarative way. For instance, to modify the ask prices for a given symbol:

```scala
def modify(symbol: Symbol)(action: TradeAction, price: Price, quantity: Quantity): TradeState =
  action match
    case TradeAction.Ask =>
      val f = Prices._Ask.modify(_.updated(price, quantity))(_)
      // ... other modifications
      TradeState.__Prices.at(symbol).modify(_.orElse(Prices.empty.some).map(g))(this)
```

This code uses the `at` optional to get the `Prices` for a `symbol`, creating a new `Prices` object if one doesn't exist, and then applies further modifications.

### Real-World Example: `At` vs. `Index`

The difference between `At` and `Index` becomes clear when we look at how `TradeState` is modified.

**Use Case for `At`: Upserting Data**

In the `modify` function, we handle incoming trade actions. A new action for a given `symbol` might be the first one we've seen, so the `prices` map might not have an entry for it yet.

```scala
// From TradeState.scala

def modify(symbol: Symbol)(action: TradeAction, price: Price, quantity: Quantity): TradeState =
  // ...
  action match
    case TradeAction.Ask =>
      // ...
      // Here we use `at` to handle both insertion and update gracefully.
      TradeState.__Prices.at(symbol).modify(_.orElse(Prices.empty.some).map(g))(this)
    // ...
```

Here, `__Prices.at(symbol)` gives us an `Optional[TradeState, Option[Prices]]`. We can then use `orElse(Prices.empty.some)` to provide a default `Prices` object if the symbol is not found. This allows us to perform an "upsert" operation: update the prices if the symbol exists, or insert a new entry if it doesn't.

**Use Case for `Index`: Modifying Existing Data**

In contrast, the `remove` function is designed to delete a price from an *existing* entry. If the symbol isn't being tracked, there's nothing to remove.

```scala
// From TradeState.scala

def remove(symbol: Symbol)(action: TradeAction, price: Price): TradeState =
  action match
    case TradeAction.Ask =>
      // __AskPrices uses `index` internally
      TradeState
        .__AskPrices(symbol)
        .modify(_.removed(price))(this)
    // ...
```

The `__AskPrices(symbol)` optional is built using `__Prices.index(s)`. This `Optional[TradeState, Prices.Ask]` will only "focus" if the symbol actually exists in the `prices` map. If it doesn't, the `modify` operation does nothing, which is exactly the desired behavior. We avoid accidentally creating a new empty entry just to perform a removal on it.

This clear separation of concerns—`At` for "upsert" and `Index` for "modify if present"—makes the state transformations more robust and predictable.

### `Traversal` for Operating on Multiple Elements

A `Traversal` is an optic that allows you to focus on zero or more values within a data structure. This is particularly useful when you want to apply a modification to all elements of a collection (like a `List`) or to a common field across different cases of an `enum`.

In `modules/domain/shared/src/main/scala/trading/events/SwitchEvent.scala`, we use a `Traversal` to create a single optic for the `CorrelationId` (`cid`) that is present in all cases of the `SwitchEvent` enum.

```scala
import monocle.Traversal
import cats.Applicative

enum SwitchEvent derives Codec.AsObject, Show:
  def id: EventId
  def cid: CorrelationId
  def createdAt: Timestamp

  case Started(
      id: EventId,
      cid: CorrelationId,
      createdAt: Timestamp
  )

  case Stopped(
      id: EventId,
      cid: CorrelationId,
      createdAt: Timestamp
  )

  case Ignored(
      id: EventId,
      cid: CorrelationId,
      createdAt: Timestamp
  )

object SwitchEvent:
  val _CorrelationId: Traversal[SwitchEvent, CorrelationId] = new:
    def modifyA[F[_]: Applicative](f: CorrelationId => F[CorrelationId])(s: SwitchEvent): F[SwitchEvent] =
      f(s.cid).map { newCid =>
        s match
          case c: Ignored => c.copy(cid = newCid)
          case c: Started => c.copy(cid = newCid)
          case c: Stopped => c.copy(cid = newCid)
      }
```

This `_CorrelationId` `Traversal` provides a unified way to `get` or `modify` the `cid` of any `SwitchEvent`, without needing to pattern match on the specific event type. It simplifies operations that need to update the correlation ID across any event.

While this example focuses on a common field in an `enum`, `Traversal` is more commonly used for collections. For example, `Traversal.fromTraverse[List, A]` would give you a `Traversal` to operate on every element of a `List[A]`.

## Conclusion

By leveraging Monocle, we can write concise, type-safe, and maintainable code for managing complex, immutable data structures. The optics provided by Monocle allow us to abstract away the boilerplate of nested updates, leading to more readable and robust code.
