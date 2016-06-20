import scalaz._
import scalaz.syntax.all._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.either._


object Sequence {

  object vanilla {
    def sequence(s: Seq[Option[Int]]): Option[Seq[Int]] = {
      val z: Option[Seq[Int]] = Some(Seq[Int]())

      val osa = s.foldLeft(z)((osa, oa) => for {
        sa <- osa
        a <- oa
      } yield sa.+:(a)
      )
      osa.map(_.reverse)
    }

    def sequenceEither(s: Seq[Either[String, Int]]): Either[String, Seq[Int]] = {
      val z: Either[String, Seq[Int]] = Right(Seq[Int]())

      val osa = s.foldLeft(z)((osa, oa) => for {
        sa <- osa.right
        a <- oa.right
      } yield sa.+:(a)
      )
      osa.right.map(_.reverse)
    }
  }

  object scalaz {
    def sequence(s: Seq[Option[Int]]): Option[List[Int]] = s.toList.sequenceU

    def sequenceEither(s: Seq[Either[String, Int]]): Either[String, List[Int]] = s.toList.sequenceU
  }
}
