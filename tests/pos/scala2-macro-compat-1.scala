

object Macro1 {
  import scala.language.experimental.macros
  def lineNumber: Int = macro LineNumberMacro2.thisLineNumberImpl
  inline def lineNumber: Int = ${ LineNumberMacro3.thisLineNumberExpr }
}

object LineNumberMacro2 {
  class Context: // Dummy scala.reflect.macros.Context
    type Expr[+T]

  def thisLineNumberImpl(context: Context): context.Expr[Int] = {
    // val lineNumber = context.enclosingPosition.line
    // context.literal(lineNumber)
    ???
  }
}

object LineNumberMacro3 {
  import scala.quoted._
  def thisLineNumberExpr(using qctx: QuoteContext): Expr[Int] = {
    import qctx.tasty.{_, given _}
    Expr(rootPosition.startLine + 1)
  }
}
