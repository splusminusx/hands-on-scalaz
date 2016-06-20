import scalaz._
import scalaz.{Reader => ZReader}

object Reader {
  trait Dao { def name = "Ahoy" }

  object vanilla {
    def service1(dao: Dao): String = dao.name
    def service2(dao: Dao): String = dao.name
    def service3(dao: Dao): String = dao.name

    def service(dao: Dao): Seq[String] = {
      Seq(
        service1(dao),
        service2(dao),
        service3(dao)
      )

    }
  }

  object scalaz {
    def service1: Reader[Dao, String] = ZReader(dao => dao.name)
    def service2: Reader[Dao, String] = ZReader(dao => dao.name)
    def service3: Reader[Dao, String] = ZReader(dao => dao.name)

    def service(dao: Dao): Seq[String] =
      (for {
        s1 <- service1
        s2 <- service2
        s3 <- service3
      } yield Seq(s1, s2, s3)).apply(dao)
  }
}
