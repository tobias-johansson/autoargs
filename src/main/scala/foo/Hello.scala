package foo

object Hello extends App {
  import Args._

  case class Config(
      foo: String,
      num: Int,
      kalle: Option[Int],
      lst: Seq[String]
  )

  implicit def seqConvert[T](implicit base: Convert[T]): Convert[Seq[T]] = Convert { src =>
    val a = src.split(',').map(base.convert)
    a.foldLeft[Option[Seq[T]]](Some(Seq())) {
      case (Some(s), Some(e)) => Some(s :+ e)
      case _                  => None
    }
  }

  val c = parse[Config](List("--num=123", "--foo=hej", "arg1", "--kalle=3", "--lst=a,b,cd"))
  println(c)

}

//
//
//
//
//
//
//
//

object Args {

  import scala.util.Try
  import shapeless._

  def flag(name: String, s: String): Either[String, String] = {
    val pattern = s"--$name=(\\S+)".r
    s match {
      case pattern(v) => Right(v)
      case _          => Left(s"No match for $name")
    }
  }

  trait Convert[T] {
    def convert(src: String): Option[T]
  }

  object Convert {
    def apply[T](c: (String) => Option[T]): Convert[T] = new Convert[T] {
      override def convert(src: String): Option[T] = c(src)
    }
  }

  implicit val intConvert: Convert[Int]       = Convert(src => Try(src.toInt).toOption)
  implicit val stringConvert: Convert[String] = Convert(src => Option(src))

  implicit def readOpt[T](implicit base: Read[T]): Read[Option[T]] = Read { (name, args) =>
    val Result(v, r) = base.read(name)(args)
    Result(Some(v), r)
  }

  implicit def read[T](implicit conv: Convert[T]): Read[T] = Read { (name, args) =>
    args.foldLeft[Result[T]](Result(None, List())) {
      case (Result(Some(v), rs), s) => Result(Some(v), rs :+ s)
      case (Result(None, rs), s) =>
        flag(name, s).right.map(conv.convert) match {
          case Right(Some(v)) => Result(Some(v), rs)
          case _              => Result(None, rs :+ s)
        }
    }
  }

  case class Result[T](
      value: Option[T],
      rest: Seq[String]
  )

  trait Read[T] { self =>

    def read(name: String)(args: Seq[String]): Result[T]

    def map[B](f: (T) => B): Read[B] = Read { (name, args) =>
      val Result(v, r) = self.read(name)(args)
      Result(v.map(f), r)
    }
  }

  object Read extends LabelledProductTypeClassCompanion[Read] {

    def apply[T](f: (String, Seq[String]) => Result[T]): Read[T] = new Read[T] {
      override def read(name: String)(args: Seq[String]): Result[T] = f(name, args)
    }

    object typeClass extends LabelledProductTypeClass[Read] {

      override def product[H, T <: HList](name: String, rh: Read[H], rt: Read[T]): Read[H :: T] =
        Read { (_, args) =>
          println(s"prodRead: $name - $args")
          val r1 @ Result(hv, hrs) = rh.read(name)(args)
          val r2 @ Result(tv, trs) = rt.read(name)(hrs)
          println(s"prodRead: $r1, $r2")
          (hv, tv) match {
            case (Some(hvv), Some(tvv)) => Result(Some(hvv :: tvv), trs)
            case _                      => Result(None, List())
          }
        }

      override def emptyProduct: Read[HNil] = Read { (name, args) =>
        Result(Some(HNil), args)
      }

      override def project[F, G](instance: => Read[G], to: (F) => G, from: (G) => F): Read[F] =
        instance.map(from)
    }
  }

  def parse[C](args: List[String])(implicit read: Read[C]) = {
    read.read("")(args)
  }

}
