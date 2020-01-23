
import scala.quoted._

object Macros {
  def impl(foo: Any) with QuoteContext : Expr[String] = Expr(foo.getClass.getCanonicalName)
}

case object Bar {
  case object Baz
}

package foo {
  case object Bar {
    case object Baz
  }
}
