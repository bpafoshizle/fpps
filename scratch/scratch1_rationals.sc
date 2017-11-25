
class Rational(x: Int, y: Int){
  def numer = x
  def denom = y

  def add(that: Rational) =
    new Rational(
      numer * that.denom + denom * that.numer,
      denom * that.denom
    )

  def neg: Rational = new Rational(-numer, denom)

  def subtract(that: Rational) = add(that.neg)

  override def toString() = numer + "/" + denom
}


var x = new Rational(1,2)
x.numer
x.denom


var y = new Rational(2,3)
x.add(y)

x = new Rational(1,3)
y = new Rational(5,7)
var z = new Rational(3,2)

x.subtract(y).subtract(z)

