
import scala.quoted._

object Macros {
  def impl(foo: Any) with QuoteContext : Expr[String] = Expr(foo.getClass.getCanonicalName)
}

class Bar {
  case object Baz
}

package foo {
  class Bar {
    case object Baz
  }
}
