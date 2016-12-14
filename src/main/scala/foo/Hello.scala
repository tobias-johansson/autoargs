package foo

import java.nio.file.Paths

import scala.util.matching.Regex

object Hello extends App {
  import Args._

  case class Config(
      foo: String,
      num: Int,
      list: Seq[Int],
      path: java.nio.file.Path,
      file: java.io.File,
      maybe: Option[Int]
  )

  val c1 = parse[Config](
    List("--num=123",
         "--foo=hello",
         "an arg",
         "--maybe=3",
         "--list=1,23,2",
         "--path=/path/to/path",
         "--file=/path/to/file"))

  val c2 = parse[Config](List("--foo=hej", "--num=x", "--list=a,b,cd"))

  show(c1)
  show(c2)
  def show(result: Result[Config]) = result match {
    case Ok(config, rest) =>
      println(s"config: $config")
      println(s"rest: $rest")

    case Fail(errors, _) =>
      println("Parameter failure:")
      println(errors.mkString("  ", "\n  ", ""))
      println("Parameters:")
      println(help[Config].mkString("  ", "\n  ", ""))

  }

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

  import scala.util.{Try, Failure, Success}
  import shapeless._

  trait Converter[T] {
    def convert(src: String): Either[String, T]
    def show: String
  }

  object Converter {
    def apply[T](shw: String)(c: (String) => Either[String, T]): Converter[T] = new Converter[T] {
      override def convert(src: String) = c(src)
      override val show                 = shw
    }
    def fromTry[T](shw: String)(c: (String) => Try[T]): Converter[T] =
      Converter(shw)(src =>
        c(src) match {
          case Success(v) => Right(v)
          case Failure(_) => Left(s"Expected <$shw>, got '$src'")
      })
    def fromOption[T](shw: String)(c: (String) => Option[T]): Converter[T] =
      Converter(shw)(src =>
        c(src) match {
          case Some(v) => Right(v)
          case None    => Left(s"Expected <$shw>, got '$src'")
      })
  }

  implicit val intConvert: Converter[Int] =
    Converter.fromTry("integer")(src => Try(src.toInt))

  implicit val stringConvert: Converter[String] =
    Converter.fromOption("string")(src => Option(src))

  implicit val pathConvert: Converter[java.nio.file.Path] =
    Converter.fromTry("path")(src => Try(Paths.get(src)))

  implicit val fileConvert: Converter[java.io.File] =
    Converter.fromTry("file")(src => Try(new java.io.File(src)))

  implicit def seqConvert[T](implicit base: Converter[T]): Converter[Seq[T]] =
    Converter(base.show + ",...") { src =>
      src.split(',').map(base.convert).foldLeft[Either[String, Seq[T]]](Right(Seq())) {
        case (Right(res), Right(n)) => Right(res :+ n)
        case (Right(_), Left(e))    => Left(e)
        case (Left(e), _)           => Left(e)
      }
    }

  implicit def readOpt[T](implicit base: Param[T]): Param[Option[T]] = Param { (name, args) =>
    base.read(name, args) match {
      case Ok(v, rs)    => Ok(Option(v), rs)
      case Fail(es, rs) => Fail(es, rs)
    }
  }

  implicit def read[T](implicit conv: Converter[T], format: Format): Param[T] = Param {
    (name, args) =>
      args.foldLeft[Result[T]](Fail[T](Seq(), List())) {
        case (Ok(v, rs), s) => Ok(v, rs :+ s)
        case (Fail(es, rs), s) =>
          format.flag(name, s).map(conv.convert) match {
            case Some(Right(v)) => Ok(v, rs)
            case Some(Left(e))  => Fail(e +: es, rs :+ s)
            case None           => Fail(es, rs :+ s)
          }
      } match {
        case Ok(v, rs)    => Ok(v, rs)
        case Fail(es, rs) => Fail(s"Unable to read parameter: $name" +: es, rs)
      }
  }

  trait Format {
    def show(name: String, tpe: String): String
    def pattern(name: String): Regex
    def flag(name: String, s: String): Option[String] = {
      val pat = pattern(name)
      s match {
        case pat(v) => Some(v)
        case _      => None
      }
    }
  }

  implicit val defaultFormat: Format = new Format {
    def show(name: String, tpe: String) = s"--$name=<$tpe>"
    def pattern(name: String): Regex    = s"--$name=(\\S+)".r
  }

  sealed trait Result[T] {
    def rest: Seq[String]
    def errors: Seq[String] = Seq()
  }
  case class Ok[T](
      value: T,
      override val rest: Seq[String]
  ) extends Result[T]
  case class Fail[T](
      override val errors: Seq[String],
      override val rest: Seq[String]
  ) extends Result[T]

  trait Param[T] { self =>

    def read(name: String, args: Seq[String]): Result[T]

    def map[B](f: (T) => B): Param[B] = Param { (name, args) =>
      self.read(name, args) match {
        case Ok(v, rs)    => Ok(f(v), rs)
        case Fail(es, rs) => Fail(es, rs)
      }
    }
  }

  object Param extends LabelledProductTypeClassCompanion[Param] {

    def apply[T](f: (String, Seq[String]) => Result[T]): Param[T] = new Param[T] {
      override def read(name: String, args: Seq[String]): Result[T] = f(name, args)
    }

    object typeClass extends LabelledProductTypeClass[Param] {

      override def product[H, T <: HList](name: String,
                                          rh: Param[H],
                                          rt: Param[T]): Param[H :: T] =
        Param { (_, args) =>
          val r1 = rh.read(name, args)
          val r2 = rt.read(name, r1.rest)
          (r1, r2) match {
            case (Ok(hv, _), Ok(tv, _)) => Ok(hv :: tv, r2.rest)
            case _                      => Fail(r1.errors ++ r2.errors, r2.rest)
          }
        }

      override def emptyProduct: Param[HNil] = Param { (name, args) =>
        Ok(HNil, args)
      }

      override def project[F, G](instance: => Param[G], to: (F) => G, from: (G) => F): Param[F] =
        instance.map(from)
    }
  }

  def parse[C](args: List[String])(implicit param: Param[C]) = {
    param.read("", args)
  }

  trait Help[T] {
    def help(name: String): Seq[String]
  }

  implicit def reqHelp[T](implicit convert: Converter[T], format: Format): Help[T] =
    new Help[T] {
      override def help(name: String): Seq[String] = Seq(format.show(name, convert.show))
    }

  implicit def optHelp[T](implicit convert: Converter[T], format: Format): Help[Option[T]] =
    new Help[Option[T]] {
      override def help(name: String): Seq[String] = Seq(s"[${format.show(name, convert.show)}]")
    }

  object Help extends LabelledProductTypeClassCompanion[Help] {

    object typeClass extends LabelledProductTypeClass[Help] {
      override def product[H, T <: HList](name: String, hh: Help[H], th: Help[T]): Help[H :: T] = {
        new Help[H :: T] {
          override def help(n: String): Seq[String] = hh.help(name) ++ th.help(n)
        }
      }

      override def emptyProduct: Help[HNil] = new Help[HNil] {
        override def help(name: String): Seq[String] = Seq()
      }

      override def project[F, G](instance: => Help[G], to: (F) => G, from: (G) => F): Help[F] =
        new Help[F] {
          override def help(name: String): Seq[String] = instance.help(name)
        }
    }
  }

  def help[C](implicit help: Help[C]): Seq[String] = help.help("")
}
