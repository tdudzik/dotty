import scala.quoted._

trait Quoted {
  def foo: Int
}
inline def quote: Quoted = ${ quoteImpl }

def quoteImpl with (qctx: QuoteContext) : Expr[Quoted] = '{
  new Quoted {
    def foo = ???
  }
}
