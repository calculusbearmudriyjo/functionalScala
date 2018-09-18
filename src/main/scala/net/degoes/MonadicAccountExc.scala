package net.degoes

//import java.util.UUID
//import io.chrisdavenport.fuuid.FUUID
import doobie._
import doobie.implicits._
import cats.effect.IO


object MonadicAccountExc {

  case class Account(id: Long, name: String, amount: Long)

  def select(name: String): ConnectionIO[Account] = sql"select * from account where name = $name".query[Account].unique
  def update(name: String, amount: Long): ConnectionIO[Int] = sql"update account set amount = $amount where name = $name".update.run

  val trx = Transactor.fromDriverManager[IO]("com.mysql.jdbc.Driver", "jdbc:mysql://localhost:3306/skeleton", "root", "123")

  def transfer(acc1: Account, acc2: Account, amount: Long) = {
//    for {
//      a1 <- select(acc1.name)
//      a2 <- select(acc2.name)
//      res1 = if (a1.amount - amount >= 0)  update(a1.name, a1.amount - amount) else sql"UPDATE 1".update
//      res2 = if (a1.amount - amount >= 0)  update(a2.name, a2.amount + amount) else sql"UPDATE 1".update
////      }
////      else 0.pure
////      Left(new Exception("amount not enough"))
//    } yield (a1, a2, res1, res2)

    select(acc1.name).flatMap(a1 => {
      select(acc2.name).flatMap(a2 => {
        if (a1.amount - amount >= 0) {
          update(a2.name, a2.amount + amount).flatMap(_ => update(a1.name, a1.amount - amount))
        } else sql"SELECT 1".query[Int].unique
      })
    })




//      if(acc2.amount >= amount) {
//        val up = update(acc1.name, acc1.amount + amount).run.flatMap(c => {
//          update(acc2.name, acc2.amount - amount).run
//        })
//        Right(up)
//      } else Left(new Exception("amount not enough"))
  }

  def main(args: Array[String]): Unit = {
    val acc1 = select("test").transact(trx).unsafeRunSync()
    val acc2 = select("test2").transact(trx).unsafeRunSync()
    val acc3 = select("test3").transact(trx).unsafeRunSync()
    val acc4 = select("test4").transact(trx).unsafeRunSync()
    val amount = 1000

    println(s"acc1 : ${acc1}, acc2: ${acc2}")

//    val op = transfer(acc1, acc2, amount)
    val r = for {
      op1 <- transfer(acc1, acc2, amount)
      op2 <- transfer(acc2, acc3, amount)
      op3 <- transfer(acc3, acc4, amount)
    } yield (op1, op2, op3)


//    println(op)
    r.transact(trx).unsafeRunSync()
//    z.transact(trx).unsafeRunSync()
//    res match {
//      case Right(op) => op.transact(trx).unsafeRunSync()
//      case Left(e) => println(e)
//    }

    val acc1u = select("test").transact(trx).unsafeRunSync()
    val acc2u = select("test2").transact(trx).unsafeRunSync()
    val acc3u = select("test3").transact(trx).unsafeRunSync()
    val acc4u = select("test4").transact(trx).unsafeRunSync()
    println(s"acc1 : ${acc1u}, acc2: ${acc2u}, acc3: ${acc3u}, acc2: ${acc4u}")

//    val p1 = transfer(acc2, acc1, amount)
//    val p2 = transfer(acc1, acc2, amount)
//    p1 match {
//      case Right(conn) => conn.transact(trx).map(_ => p2 {
//        case Right(conn2) => conn2
//      })//println(s"res : ${conn.transact(trx).unsafeRunSync()}")
//      case Left(e) => println(e)
//    }

  }
  //    println(p.transact(trx).unsafeRunSync())
  //    def uuid =  IO.apply(java.util.UUID.randomUUID())//FUUID.randomFUUID[IO]
  //    val uuid2: UUID = java.util.UUID.randomUUID()
  //
  //    println(uuid + " " + uuid)
  //    println(uuid == uuid)
  //    println(uuid2 == uuid2)
  //
  //    println(FUUID.randomFUUID[IO] == FUUID.randomFUUID[IO])
  //    println(java.util.UUID.randomUUID == java.util.UUID.randomUUID)
  //
  //    val equalToItself : IO[Boolean] = for {
  //      fuuid <- uuid
  //      fuuid2 <- uuid
  //    } yield fuuid == fuuid

  //    println(equalToItself.unsafeRunSync)

  //    println(uuid == uuid)
  //    val res = for {
  //     a <- uuid
  //    } yield a
}
