package se.tobjoh

import java.nio.file.Paths

import shapeless._

import scala.util.matching.Regex
import scala.util.Try

package object autoargs {

  sealed trait Result[T] {
    def rest: Seq[String]
    def errors: Seq[String] = Seq()
  }

  case class Good[T](
      value: T,
      override val rest: Seq[String]
  ) extends Result[T]

  case class Bad[T](
      override val errors: Seq[String],
      override val rest: Seq[String]
  ) extends Result[T]

  trait Converter[T] {
    def convert(src: String): Either[String, T]
    def help: String
  }

  object Converter {
    def apply[T](hlp: String)(c: (String) => Either[String, T]): Converter[T] = new Converter[T] {
      override def convert(src: String) = c(src)
      override val help                 = hlp
    }

    def fromTry[T](hlp: String)(c: (String) => Try[T]): Converter[T] =
      Converter.fromOption(hlp)(src => c(src).toOption)

    def fromOption[T](hlp: String)(c: (String) => Option[T]): Converter[T] =
      Converter(hlp)(src =>
        c(src) match {
          case Some(v) => Right(v)
          case None    => Left(s"Expected: <$hlp>, found: $src")
      })

    implicit val stringConverter: Converter[String] =
      Converter.fromOption("string")(src => Option(src))

    implicit val boolConverter: Converter[Boolean] =
      Converter.fromTry("boolean")(src => Try(src.toBoolean))

    implicit val intConverter: Converter[Int] =
      Converter.fromTry("integer")(src => Try(src.toInt))

    implicit val longConverter: Converter[Long] =
      Converter.fromTry("long")(src => Try(src.toLong))

    implicit val floatConverter: Converter[Float] =
      Converter.fromTry("float")(src => Try(src.toFloat))

    implicit val doubleConverter: Converter[Double] =
      Converter.fromTry("double")(src => Try(src.toDouble))

    implicit val pathConverter: Converter[java.nio.file.Path] =
      Converter.fromTry("path")(src => Try(Paths.get(src)))

    implicit val fileConverter: Converter[java.io.File] =
      Converter.fromTry("file")(src => Try(new java.io.File(src)))

    implicit def seqConverter[T](implicit base: Converter[T]): Converter[Seq[T]] =
      Converter(base.help + ",...") { src =>
        src.split(',').map(base.convert).foldLeft[Either[String, Seq[T]]](Right(Seq())) {
          case (Right(res), Right(n)) => Right(res :+ n)
          case (Right(_), Left(e))    => Left(e)
          case (Left(e), _)           => Left(e)
        }
      }
  }

  trait Format {
    def help(name: String, tpe: String): String

    def pattern(name: String): Regex

    def flag(name: String, s: String): Option[String] = {
      val pat = pattern(name)
      s match {
        case pat(v) => Some(v)
        case _      => None
      }
    }
  }

  object Format {
    implicit val defaultFormat: Format = new Format {
      def help(name: String, tpe: String) = s"--$name=<$tpe>"
      def pattern(name: String): Regex    = s"--$name=(\\S+)".r
    }
  }

  trait Reader[T] { self =>
    def read(name: String, args: Seq[String]): Result[T]
    def map[B](f: (T) => B): Reader[B] = Reader { (name, args) =>
      self.read(name, args) match {
        case Good(v, rs) => Good(f(v), rs)
        case Bad(es, rs) => Bad(es, rs)
      }
    }
  }

  object Reader extends LabelledProductTypeClassCompanion[Reader] {

    def apply[T](f: (String, Seq[String]) => Result[T]): Reader[T] = new Reader[T] {
      override def read(name: String, args: Seq[String]): Result[T] = f(name, args)
    }

    implicit def optReader[T](implicit base: Reader[T]): Reader[Option[T]] = Reader { (name, args) =>
      base.read(name, args) match {
        case Good(v, rs) => Good(Option(v), rs)
        case Bad(es, rs) => Good(None, rs)
      }
    }

    implicit def reqReader[T](implicit conv: Converter[T], format: Format): Reader[T] =
      Reader { (name, args) =>
        args.foldLeft[Result[T]](Bad[T](Seq(), List())) {
          case (Good(v, rs), s) => Good(v, rs :+ s)
          case (Bad(es, rs), s) =>
            format.flag(name, s).map(conv.convert) match {
              case Some(Right(v)) => Good(v, rs)
              case Some(Left(e))  => Bad(e +: es, rs :+ s)
              case None           => Bad(es, rs :+ s)
            }
        } match {
          case Good(v, rs) => Good(v, rs)
          case Bad(es, rs) => Bad(s"Unable to read argument: ${format.help(name, conv.help)}" +: es, rs)
        }
      }

    object typeClass extends LabelledProductTypeClass[Reader] {

      override def product[H, T <: HList](name: String, rh: Reader[H], rt: Reader[T]): Reader[H :: T] =
        Reader { (_, args) =>
          val r1 = rh.read(name, args)
          val r2 = rt.read(name, r1.rest)
          (r1, r2) match {
            case (Good(hv, _), Good(tv, _)) => Good(hv :: tv, r2.rest)
            case _                          => Bad(r1.errors ++ r2.errors, r2.rest)
          }
        }

      override def emptyProduct: Reader[HNil] = Reader { (_, args) =>
        Good(HNil, args)
      }

      override def project[F, G](instance: => Reader[G], to: (F) => G, from: (G) => F): Reader[F] =
        instance.map(from)
    }
  }

  def parse[C](args: List[String])(implicit param: Reader[C]) = {
    param.read("", args)
  }

  trait Help[T] {
    def help(name: String): Seq[String]
  }

  object Help extends LabelledProductTypeClassCompanion[Help] {

    implicit def reqHelp[T](implicit converter: Converter[T], format: Format): Help[T] =
      new Help[T] {
        override def help(name: String): Seq[String] = Seq(format.help(name, converter.help))
      }

    implicit def optHelp[T](implicit converter: Converter[T], format: Format): Help[Option[T]] =
      new Help[Option[T]] {
        override def help(name: String): Seq[String] =
          Seq(s"[${format.help(name, converter.help)}]")
      }

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
