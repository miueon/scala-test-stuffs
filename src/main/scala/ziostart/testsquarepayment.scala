import zio.ZIOAppDefault
import zio.Scope
import zio.ZIO
import zio.ZIOAppArgs
import com.squareup.square.SquareClient
import com.squareup.square.models.CreateCustomerRequest
import com.squareup.square.models.CreateCardRequest
import com.squareup.square.models.Card
import com.squareup.square.models.Address
import com.squareup.square.models.CreatePaymentRequest
import java.util.UUID
import com.squareup.square.models.Money
import com.squareup.square.authentication.BearerAuthModel
import zio.Console.*
import java.util.concurrent.CompletionException
import com.squareup.square.exceptions.ApiException
import java.io.IOException

object Main extends ZIOAppDefault:
  // Create a Square Client
  val squareClient = new SquareClient.Builder()
    .environment(com.squareup.square.Environment.PRODUCTION)
    .accessToken("EAAAlolmglHJKvA291uqMoDl7jZQo_yapO0ClbJmCIHpQ6ICQKfMisBxFHvtfKO7")
    .build()

  // Create a Payment
  val createPaymentRequest =
    CreatePaymentRequest
      .Builder("cnon:CBASENjTJfGYdzKNEkM27d3VGe8", UUID.randomUUID().toString())
      .amountMoney(
        Money
          .Builder()
          .amount(6L) // $20.00
          .currency("USD")
          .build()
      )
      .appFeeMoney(
        Money
          .Builder()
          .amount(1L) // $2.00
          .currency("USD")
          .build()
      )
      .orderId(UUID.randomUUID().toString())
      .idempotencyKey(UUID.randomUUID().toString())
      .build()
  def run: ZIO[Any & (ZIOAppArgs & Scope), Any, Any] =
    val logic = for
      payment <- ZIO.attempt(squareClient.getPaymentsApi().createPayment(createPaymentRequest))
      _ <- printLine(payment.getErrors())
      _ <- printLine(payment.toString())
    yield ()

    // logic.catchAll{
    //   case e if e.isInstanceOf[ApiException] => 
    //     printLine(e.getCause()).flatMap(_ => printLine(e))
    //   case e if e.isInstanceOf[IOException] => 
    //     printLine(e.getMessage())
    // }
    logic
end Main
