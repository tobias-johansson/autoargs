package se.tobjoh.autoargs

import org.scalatest._

class Usage extends FreeSpec with Matchers {

  "All simple types are parsed" in {

    case class Config(
        string: String,
        int: Int,
        long: Long,
        boolean: Boolean,
        float: Float,
        double: Double
    )

    val args = List(
      "--string=message",
      "--int=123",
      "--long=5431",
      "--boolean=true",
      "--float=3.14",
      "--double=2.4e4"
    )

    parse[Config](args) shouldBe
      Good(Config(
             string = "message",
             int = 123,
             long = 5431,
             boolean = true,
             float = 3.14f,
             double = 2.4e4
           ),
           Seq())
  }

  "A few other types are parsed" in {

    case class Config(
        path: java.nio.file.Path,
        file: java.io.File
    )

    val args = List(
      "--path=/path/as/path",
      "--file=/path/as/file"
    )

    parse[Config](args) shouldBe
      Good(Config(
             path = java.nio.file.Paths.get("/path", "as", "path"),
             file = new java.io.File("/path/as/file")
           ),
           Seq())
  }

  "Parsing fails when args are given values of wrong type" in {

    case class Config(
        size: Int,
        extra: Option[Float]
    )

    parse[Config](List("--size=foo", "--extra=bar")) shouldBe
      Bad(Seq(
            "Unable to read argument: --size=<integer>",
            "Expected: <integer>, found: foo"
          ),
          Seq(
            "--size=foo",
            "--extra=bar"
          ))

  }

  "Parsing fails when required args are missing" in {

    case class Config(
        name: String,
        size: Int
    )

    parse[Config](List("--name=jeb")) shouldBe
      Bad(Seq("Unable to read argument: --size=<integer>"), Seq())

    parse[Config](List("--size=123")) shouldBe
      Bad(Seq("Unable to read argument: --name=<string>"), Seq())

    parse[Config](List()) shouldBe
      Bad(Seq(
            "Unable to read argument: --name=<string>",
            "Unable to read argument: --size=<integer>"
          ),
          Seq())
  }

  "Optional args are expressed using Option[T]" in {

    case class Config(opt: Option[Int])

    parse[Config](List()) shouldBe
      Good(Config(opt = None), Seq())

    parse[Config](List("--opt=31")) shouldBe
      Good(Config(opt = Some(31)), Seq())
  }

  "Multi-value args are expressed using Seq[T]" in {

    case class Config(vals: Seq[Int])

    parse[Config](List("--vals=9,67,3")) shouldBe
      Good(Config(vals = Seq(9, 67, 3)), Seq())
  }

  "Argument help is generated" in {

    case class Config(
        string: String,
        int: Int,
        opt: Option[Int]
    )

    help[Config] shouldBe Seq(
      "--string=<string>",
      "--int=<integer>",
      "[--opt=<integer>]"
    )
  }
}
