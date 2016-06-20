import scalaz._
import scalaz.OptionT._
import scalaz.std.option._
import scalaz.std.scalaFuture._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object FutureOption {
  // def findAmountById(id: String) : Future[Option[Int]]

  object vanilla {
    def flatMap(fo1: Future[Option[Int]], fo2: Future[Option[Int]]): Future[Option[Int]] = {
      for {
        o1 <- fo1
        o2 <- fo2
      } yield {
        for {
          i1 <- o1
          i2 <- o2
        } yield i1 + i2
      }
    }
  }

  object scalaz {
    def flatMap(fo1: Future[Option[Int]], fo2: Future[Option[Int]]): Future[Option[Int]] = {
      (for {
        o1 <- optionT(fo1)
        o2 <- optionT(fo2)
      } yield o1 + o2).run
    }
  }

}
