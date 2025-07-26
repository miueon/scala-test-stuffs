package monocle.illustration

import cats.effect.*
import cats.syntax.all.*
import monocle.Traversal
import cats.Applicative

/** Traversal Illustration \======================
  *
  * Traversal allows us to focus on multiple values within a structure and modify them all at once using an Applicative
  * functor.
  *
  * Structure: Container[A] ----Traversal----> 0..n values of type A
  *
  * Key Properties:
  *   - Can access zero, one, or many values
  *   - Uses Applicative for composing effects
  *   - Preserves structure while transforming values
  */

// Example: Stock Portfolio with multiple holdings
case class Holding(symbol: String, shares: Int, pricePerShare: Double):
  def value: Double = shares * pricePerShare

case class Portfolio(holdings: List[Holding], cash: Double)

object Portfolio:
  // Traversal to access all holdings
  val allHoldings: Traversal[Portfolio, Holding] = new:
    def modifyA[F[_]: Applicative](f: Holding => F[Holding])(portfolio: Portfolio): F[Portfolio] =
      portfolio.holdings.traverse(f).map(newHoldings => portfolio.copy(holdings = newHoldings))

  // Traversal to access share counts of all holdings
  val allShares: Traversal[Portfolio, Int] = new:
    def modifyA[F[_]: Applicative](f: Int => F[Int])(portfolio: Portfolio): F[Portfolio] =
      portfolio.holdings
        .traverse(holding => f(holding.shares).map(newShares => holding.copy(shares = newShares)))
        .map(newHoldings => portfolio.copy(holdings = newHoldings))

  // Traversal to access prices of all holdings
  val allPrices: Traversal[Portfolio, Double] = new:
    def modifyA[F[_]: Applicative](f: Double => F[Double])(portfolio: Portfolio): F[Portfolio] =
      portfolio.holdings
        .traverse(holding => f(holding.pricePerShare).map(newPrice => holding.copy(pricePerShare = newPrice)))
        .map(newHoldings => portfolio.copy(holdings = newHoldings))
end Portfolio

/** Visual Representation:
  *
  * Portfolio ├── holdings: List[Holding] │ ├── Holding("AAPL", 100, 150.0) ←─┐ │ ├── Holding("GOOGL", 50, 2800.0) ←─┤
  * Traversal focuses here │ └── Holding("TSLA", 75, 900.0) ←─┘ └── cash: 10000.0
  *
  * Traversal Operations:
  *
  *   1. VIEW ALL: Extract all focused values Portfolio.allShares.getAll(portfolio) → List(100, 50, 75)
  *
  * 2. MODIFY ALL: Transform all focused values Portfolio.allShares.modify(_ * 2)(portfolio) → doubles all share counts
  *
  * 3. EFFECTFUL MODIFY: Apply effects while transforming Portfolio.allPrices.modifyA(price => IO(price *
  * 1.1))(portfolio) → increases all prices by 10% with IO effects
  */

object TraversalDemo extends IOApp.Simple:

  val samplePortfolio = Portfolio(
    holdings = List(
      Holding("AAPL", 100, 150.0),
      Holding("GOOGL", 50, 2800.0),
      Holding("TSLA", 75, 900.0)
    ),
    cash = 10000.0
  )

  def run: IO[Unit] =
    for
      _ <- IO.println("=== Traversal Illustration ===\n")

      _ <- IO.println("Original Portfolio:")
      _ <- IO.println(s"Holdings: ${samplePortfolio.holdings}")
      _ <- IO.println(s"Cash: ${samplePortfolio.cash}")
      _ <- IO.println(s"Total Value: ${samplePortfolio.holdings.map(_.value).sum + samplePortfolio.cash}\n")

      // Example 1: View all share counts
      _ <- IO.println("=== Viewing All Share Counts ===")
      allShares = Portfolio.allShares.getAll(samplePortfolio)
      _ <- IO.println(s"All share counts: $allShares\n")

      // Example 2: Modify all share counts (double them)
      _ <- IO.println("=== Doubling All Share Counts ===")
      doubledShares = Portfolio.allShares.modify(_ * 2)(samplePortfolio)
      _ <- IO.println(s"After doubling shares:")
      _ <- doubledShares.holdings.traverse(h => IO.println(s"  ${h.symbol}: ${h.shares} shares"))

      // Example 3: Effectful modification (price increase with logging)
      _ <- IO.println("=== Increasing All Prices by 10% (with effects) ===")
      increasedPrices <- Portfolio.allPrices.modifyA { price =>
        for _ <- IO.println(s"  Updating price from $price to ${price * 1.1}")
        yield price * 1.1
      }(samplePortfolio)

      _ <- IO.println("Final portfolio:")
      _ <- increasedPrices.holdings.traverse(h =>
        IO.println(s"  ${h.symbol}: ${h.shares} shares @ ${h.pricePerShare} = ${h.value}")
      )
    yield ()
end TraversalDemo

/** Key Insights:
  *
  *   1. Traversal vs Other Optics:
  *      - Lens: 1 → 1 (exactly one value)
  *      - Optional: 1 → 0|1 (maybe one value)
  *      - Traversal: 1 → 0|1|many (zero or more values)
  *
  * 2. Applicative Requirement:
  *   - Allows combining effects when modifying multiple values
  *   - Identity for pure modifications
  *   - IO/Future/etc for effectful modifications
  *
  * 3. Structure Preservation:
  *   - Original container structure is maintained
  *   - Only the focused values are transformed
  *
  * 4. Composition:
  *   - Traversals compose with other optics
  *   - Can chain multiple traversals for nested structures
  */
