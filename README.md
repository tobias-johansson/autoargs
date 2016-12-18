# autoargs

Scala library for parsing simple command line arguments with a bare minimum of boilerplate.

## Usage

Expected arguments are declared as a case class.
Parsers are automatically derived from the field names and types.

```scala
scala> import se.tobjoh.autoargs._
import se.tobjoh.autoargs._

scala> case class Config(foo: String, bar: Seq[Int], baz: Option[Boolean])
defined class Config

scala> parse[Config](List("--foo=hello", "--bar=5,23,1"))
res1: se.tobjoh.autoargs.Result[Config] = Good(Config(hello,List(5, 23, 1),None),List())
```

On failure to parse, an accumulated list of error messages is available.
```scala
scala> parse[Config](List())
res2: se.tobjoh.autoargs.Result[Config] = Bad(List(Unable to read parameter: foo, Unable to read parameter: bar),List())
```

Limited argument help messages can be generated.
```scala
scala> help[Config].mkString("\n")
res3: String =
--foo=<string>
--bar=<integer,...>
[--baz=<boolean>]
```

## Future work

Ideas:
- Fail on unrecognized arguments
- Support argument forms like `-f <value>`
- Support mixed argument forms using `@@` tags or similar
- Add argument description strings using singleton types
