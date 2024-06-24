package stm

// import cats.effect.IOApp
// import cats.effect.IO
// import io.github.timwspence.cats.stm.STM

// object stmTest extends IOApp.Simple:
//   def wibble(stm: STM[IO])(tvar: stm.TVar[Int]): stm.Txn[Int] =
//     for
//       current <- tvar.get
//       updated = current + 1
//       _ <- tvar.set(updated)
//     yield updated

//   def run: IO[Unit] =
//     def r(stm: STM[IO]): IO[Int] =
//       import stm.*
//       for
//         tvar <- stm.commit(TVar.of(0))
//         res <- stm.commit(wibble(stm)(tvar))
//       yield res

//     for 
//       result <- STM.runtime[IO].flatMap(r(_))
//       _ <- IO.print(result)
//     yield ()

