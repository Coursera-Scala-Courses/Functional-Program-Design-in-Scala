package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for{
      (name,expr) <- namedExpressions
    }yield name -> Signal(eval(expr(),namedExpressions))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match{
    //Refs to other variables could cause cyclic dependencies (e.g., a = b + 1 and b = 2 * a.
    // Such cyclic dependencies are considered as errors (failing to detect this will cause infinite loops).
    case Literal(v) => v
    case Ref(name) => eval(getReferenceExpr(name,references),references-name)
    case Plus(expr1, expr2) => eval(expr1, references) + eval(expr2, references)
    case Minus(expr1, expr2) => eval(expr1, references) - eval(expr2, references)
    case Times(expr1, expr2) => eval(expr1, references) * eval(expr2, references)
    case Divide(expr1, expr2) => eval(expr1, references) / eval(expr2, references)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
