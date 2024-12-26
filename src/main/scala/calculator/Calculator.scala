package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface:
 import Expr.*

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    namedExpressions.map { case (name, signalExpr) =>
      (name, Signal(eval(signalExpr(), namedExpressions.removed(name))))
    }

  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double = {
    def evalHelper(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
      case Divide(a, b) =>
        if (evalHelper(b, references) == 0) Double.NaN else evalHelper(a, references) / evalHelper(b, references)

      case Literal(v) => v

      case Minus(a, b) =>
        evalHelper(a, references) - evalHelper(b, references)

      case Plus(a, b) =>
        evalHelper(a, references) + evalHelper(b, references)

      case Ref(name) =>
        val refExpr = getReferenceExpr(name, references)
        evalHelper(refExpr, references.removed(name))

      case Times(a, b) =>
        evalHelper(a, references) * evalHelper(b, references)
    }

    evalHelper(expr, references)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]])(using Signal.Caller): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
