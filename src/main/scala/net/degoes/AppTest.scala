package net.degoes

import scalaz.zio.{IO, console}

object AppTest extends scalaz.zio.App {

  case class Account(balance: Balance)
  type Adjustment = Long
  case class Balance(value: Long)

  trait Bank {
    def balance(account: Account): IO[Nothing, Balance]

    def adjust(account: Account, adjustment: Adjustment): IO[Exception, Balance]

    def transaction(fa: IO[Exception, (Balance, Balance)]): IO[Exception, (Balance, Balance)]
  }

  object Bank {
    implicit val BankIO = new Bank { self =>
      def balance(account: Account): IO[Nothing, Balance] = IO.point(account.balance)
      def adjust(account: Account, adjustment: Adjustment): IO[Exception, Balance] = IO.syncException({
        if (account.balance.value < adjustment) {
          throw new Exception(s"balance : ${account.balance.value}, lower that: ${adjustment}")
        } else {
          Balance(account.balance.value - adjustment)
        }
      })

      def transaction(fa: IO[Exception, (Balance, Balance)]): IO[Exception, (Balance, Balance)] = fa
    }
  }

  def run(args: List[String]): IO[Nothing, ExitStatus] = {
    import Bank.BankIO._
    val account1 = Account(Balance(300))
    val account2 = Account(Balance(300))
//    val result=
    (for {
    resT <- transaction {
        for {
            _ <- console.putStrLn("test ")
            b2 <- adjust(account2, -200)
            _ <- console.putStrLn(s"acc2: ${b2.value}")
            b1 <- adjust(account1, 400)
            _ <- console.putStrLn(s"acc1: ${b1.value}, acc2: ${b2.value}")
          } yield (b1, b2)}
    _ <- console.putStrLn(s"acc1: ${resT._1.value}, acc2: ${resT._2.value}")
    } yield resT).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))

//    result.run(r => { println(r._1.value) })
  }

}
