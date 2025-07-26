package atandfocus

import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.function.At
import monocle.function.Index
import cats.syntax.all.*
import cats.effect.IOApp
import cats.effect.IO
import monocle.Traversal
import cats.Applicative

type Ask = Map[Int, Int]
type Bid = Map[Int, Int]

case class Prices(
    ask: Ask,
    bid: Bid,
    high: Int,
    low: Int
)

object Prices:
  def empty = Prices(Map.empty, Map.empty, 0, 0)
  val _Ask = Focus[Prices](_.ask)
  val _Bid = Focus[Prices](_.bid)
  val _High = Focus[Prices](_.high)
  val _Low = Focus[Prices](_.low)

case class TradeState(status: String, prices: Map[String, Prices]):
  def modify(symbol: String)(action: TradeAction, price: Int, quantity: Int): TradeState =
    val h = Prices._High.modify(p => if price > p then price else p)
    val l = Prices._Low.modify(p => if price < p then price else p)
    action match
      case TradeAction.Ask =>
        val f = Prices._Ask.modify(_.updated(price, quantity))
        val g = f.andThen(h).andThen(l)
        TradeState.__Prices.at(symbol).modify(_.orElse(Prices.empty.some).map(g))(this)
      case _ => ???

  def remove(symbol: String)(action: TradeAction, price: Int): TradeState =
    action match
      case atandfocus.TradeAction.Ask =>
        TradeState.__AskPrices(symbol).modify(_.removed(price))(this)
      case _ => ???

enum SwitchEvent:
  def id: Int 
  def cid: Int
  def createdAt: String

  case Started(
      id: Int,
      cid: Int,
      createdAt: String 
  )

  case Stopped(
      id: Int,
      cid: Int,
      createdAt: String
  )

  case Ignored(
      id: Int,
      cid: Int,
      createdAt: String 
  )

object SwitchEvent:
  val _CorrelationId: Traversal[SwitchEvent, Int] = new:
    def modifyA[F[_]: Applicative](f: Int => F[Int])(s: SwitchEvent): F[SwitchEvent] = 
      f(s.cid).map { newCid =>
        s match
          case SwitchEvent.Started(id, _, createdAt) => SwitchEvent.Started(id, newCid, createdAt)
          case SwitchEvent.Stopped(id, _, createdAt) => SwitchEvent.Stopped(id, newCid, createdAt)
          case SwitchEvent.Ignored(id, _, createdAt) => SwitchEvent.Ignored(id, newCid, createdAt)
      }

object TradeState:
  val _Status = Focus[TradeState](_.status)
  val _Prices = Focus[TradeState](_.prices)

  object __Prices:
    def at(s: String): Optional[TradeState, Option[Prices]] =
      _Prices.andThen(At.atMap[String, Prices].at(s))

    def index(s: String): Optional[TradeState, Prices] =
      _Prices.andThen(Index.mapIndex[String, Prices].index(s))

  object __AskPrices:
    def apply(s: String): Optional[TradeState, Ask] =
      __Prices.index(s).andThen(Prices._Ask)

  object __BidPrices:
    def apply(s: String): Optional[TradeState, Bid] =
      __Prices.index(s).andThen(Prices._Bid)

enum TradeAction:
  case Ask
  case Bid

object atandfocusTest extends IOApp.Simple:
  def run: IO[Unit] =
    // Create initial trade state
    val initialState = TradeState(
      "active",
      Map(
        "AAPL" -> Prices(Map(150 -> 100, 155 -> 50), Map(145 -> 75, 140 -> 30), 160, 140),
        "GOOGL" -> Prices(Map(2800 -> 10), Map(2750 -> 5), 2850, 2700)
      )
    )

    // Print initial state
    for
      _ <- IO.println("=== Initial Trade State ===")
      _ <- IO.println(s"Status: ${initialState.status}")
      _ <- IO.println(s"AAPL prices: ${initialState.prices.get("AAPL")}")
      _ <- IO.println(s"GOOGL prices: ${initialState.prices.get("GOOGL")}")
      _ <- IO.println("")

      // Demonstrate modifying prices using optics
      _ <- IO.println("=== Modifying AAPL Ask Prices ===")
      newState = initialState.modify("AAPL")(TradeAction.Ask, 152, 75)
      _ <- IO.println(s"Updated AAPL ask prices: ${newState.prices.get("AAPL").map(_.ask)}")
      _ <- IO.println("")

      // Demonstrate using lenses to view specific fields
      _ <- IO.println("=== Using Lenses to View Fields ===")
      aaplPrices = initialState.prices.get("AAPL").getOrElse(Prices.empty)
      highPrice = Prices._High.get(aaplPrices)
      lowPrice = Prices._Low.get(aaplPrices)
      _ <- IO.println(s"AAPL high price: $highPrice")
      _ <- IO.println(s"AAPL low price: $lowPrice")
      _ <- IO.println("")

      // Demonstrate using Optional to safely access nested data
      _ <- IO.println("=== Using Optional to Access Nested Data ===")
      maybeAAPL = TradeState.__Prices.at("AAPL").headOption(initialState)
      maybeTSLA = TradeState.__Prices.at("TSLA").headOption(initialState)
      _ <- IO.println(s"AAPL exists: ${maybeAAPL.isDefined}")
      _ <- IO.println(s"TSLA exists: ${maybeTSLA.isDefined}")
      aaplRemoved = initialState.remove("AAPL")(TradeAction.Ask, 150)
      _ <- IO.println(s"AAPL removed: ${aaplRemoved.prices.get("AAPL")}")
      swithEvent = SwitchEvent.Started(1, 2, "2023-08-01")
      
    yield ()
    end for
  end run
end atandfocusTest
