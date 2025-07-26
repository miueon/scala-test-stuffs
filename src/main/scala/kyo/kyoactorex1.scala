
import java.io.IOException
import java.util.concurrent.locks.LockSupport
import kyo.*

enum Kind:
  case Deposit, Withdraw, Interest

object Logger:
  sealed trait Message

  case class Transaction(
      account: Int,
      kind: Kind,
      amount: Double,
      balance: Double
  ) extends Message

  case class Sync(replyTo: Subject[Unit]) extends Message

  def init(using Frame): Actor[IOException, Message, Unit] < Async =
    Actor.run {
      Var.run(Chunk.empty[Transaction]) {
        Poll[Message] {
          case transaction: Transaction =>
            Console
              .println(transaction)
              .andThen(
                Var.updateDiscard[Chunk[Transaction]](_.append(transaction))
              )
          case Sync(replyto) =>
            replyto.send(())
        }
      }
    }
end Logger

object Account:
  sealed trait Message
  case class Deposit(amount: Double) extends Message
  case class Withdraw(amount: Double, replyTo: Subject[WithdrawResult]) extends Message
  case class GetBalance(replyTo: Subject[Balance]) extends Message
  case class ApplyInterest(rate: Double) extends Message

  case class WithdrawResult(success: Boolean, msg: String, balance: Double)
  case class Balance(balance: Double)

  def init(id: Int, logger: Subject[Logger.Message])(using Frame) =
    Actor.run {
      Var.run(0d) {
        Poll[Message] {
          case GetBalance(replyTo) =>
            Var.use[Double](blance => replyTo.send(Balance(blance)))

          case Deposit(amount) =>
            for
              newBalance <- Var.update[Double](_ + amount)
              _ <- logger.send(Logger.Transaction(id, Kind.Deposit, amount, newBalance))
            yield ()

          case Withdraw(amount, replyTo) =>
            Var.use[Double] { currentBalance =>
              if currentBalance < amount then replyTo.send(WithdrawResult(false, "Insufficient funds", currentBalance))
              else
                for
                  newBalance <- Var.update[Double](_ - amount)
                  _ <- logger.send(Logger.Transaction(id, Kind.Withdraw, amount, newBalance))
                  _ <- replyTo.send(WithdrawResult(true, "Withdraw successful", newBalance))
                yield ()
            }
          case ApplyInterest(rate) =>
            for
              balance <- Var.get[Double]
              interest = balance * rate
              newBalance = balance + interest
              _ <- Var.set(newBalance)
              _ <- logger.send(Logger.Transaction(id, Kind.Interest, interest, newBalance))
            yield ()
        }
      }
    }
end Account

case class Bank(
    logger: Subject[Logger.Message],
    nextTransactionId: AtomicLong,
    nextAccountId: AtomicInt
):
  def newAccount(using Frame): Subject[Account.Message] < Async =
    nextAccountId.incrementAndGet.map(Account.init(_, logger).map(_.subject))


object Bank:
  def init(using Frame): Bank < Async = 
    for 
      logger <- Logger.init
      nextTransactionId <- AtomicLong.init
      nextAccountId <- AtomicInt.init
    yield Bank(logger.subject, nextTransactionId, nextAccountId)

object Demo extends KyoApp:
  run {
    for
      bank <- Bank.init
      account1 <- bank.newAccount
      _ <- Async.parallelUnbounded((0 until 100).map(_ => account1.send(Account.Deposit((1)))))
      _ <- Async.parallelUnbounded((0 until 50).map(_ => account1.ask(Account.Withdraw(1, _))))
      _ <- bank.logger.ask(Logger.Sync(_))
      response <- account1.ask(Account.GetBalance(_))
    yield response
  }