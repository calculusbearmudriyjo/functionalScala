package net.degoes

import scalaz.zio._
import scalaz.zio.console._

object FunctionalScala extends App {
  def run(args: List[String]): IO[Nothing, ExitStatus] = {
    val a: List[Int => Int] = List((x: Int) => x * x, (y: Int) => y + 2)

    (for {
      _ <- putStrLn("Hello World!")
    } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))
  }
}
