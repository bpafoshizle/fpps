
Sum(Number(1), Number(44)).show
Sum(Prod(Number(2), Var("x")), Var("y")).show
Prod(Sum(Number(2), Var("x")), Var("y")).show
Prod(Var("y"), Sum(Number(2),Var("x"))).show

trait Expr {
  def isNumber: Boolean
  def isSum: Boolean

  def isProd: Boolean
  def isVar: Boolean
  def varValue: String

  def numValue: Int
  def leftOp: Expr
  def rightOp: Expr

  def show: String = this match {
    case Number(n) => n.toString
    case Sum(e1, e2) => e1.show + "+" + e2.show
    case Prod(Sum(e1,e2), e3) => "(" + Sum(e1,e2).show + ")" + "*"  + e3.show
    case Prod(e1, Sum(e2,e3)) => e1.show + "*" + "(" + Sum(e2,e3).show + ")"
    case Prod(e1, e2) => e1.show + "*" + e2.show
    case Var(x) => x
  }
}

case class Number(n: Int)extends Expr {
  def isNumber: Boolean = true
  def isSum: Boolean = false

  def isProd: Boolean = false
  def isVar: Boolean = false
  def varValue: String = throw new Error("Number.varValue")

  def numValue: Int = n
  def leftOp: Expr = throw new Error("Number.leftOp")
  def rightOp: Expr = throw new Error("Number.rightOp")
}

case class Sum(e1: Expr, e2: Expr)extends Expr {
  def isNumber: Boolean = false
  def isSum: Boolean = true

  def isProd: Boolean = false
  def isVar: Boolean = false
  def varValue: String = throw new Error("Sum.varValue")

  def numValue: Int = throw new Error("Sum.numValue")
  def leftOp: Expr = e1
  def rightOp: Expr = e2
}

case class Prod(e1: Expr, e2: Expr)extends Expr {
  def isNumber: Boolean = false
  def isSum: Boolean = false
  def isProd: Boolean = true
  def isVar: Boolean = false
  def varValue: String = throw new Error("Prod.varValue")
  def numValue: Int = throw new Error("Prod.numValue")
  def leftOp: Expr = e1
  def rightOp: Expr = e2
}

case class Var(x: String)extends Expr {
  def isNumber: Boolean = false
  def isSum: Boolean = false
  def isProd: Boolean = false
  def isVar: Boolean = true
  def varValue: String = x
  def numValue: Int = throw new Error("Var.numValue")
  def leftOp: Expr = throw new Error("Var.leftOp")
  def rightOp: Expr = throw new Error("Var.rightOp")
}

def eval(e: Expr): Int = {
  if(e.isNumber) e.numValue
  else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
  else throw new Error("Unknown expression " + e)
}

