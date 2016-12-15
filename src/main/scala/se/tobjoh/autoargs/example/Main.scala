package se.tobjoh.autoargs.example

import se.tobjoh.autoargs._

object Main extends App {

  case class Config(
      string: String,
      num: Int,
      flag: Boolean,
      double: Double,
      list: Seq[Int],
      path: java.nio.file.Path,
      maybe: Option[Int]
  )

  val Good(config, rest) =
    parse[Config](
      List("--string=foo",
           "--num=123",
           "--flag=true",
           "--double=1.5e3",
           "--list=1,23,2",
           "--path=/path/to/path",
           "an arg"))

  println(s"Config: $config")
  println(s"Rest: $rest")

  val Bad(errors, _) =
    parse[Config](List("--foo=hej", "--num=x", "--list=a,b,cd"))

  println("Parameter failure:")
  println(errors.map("  " + _).mkString("\n"))
  println("Parameters:")
  println(help[Config].map("  " + _).mkString("\n"))

}
