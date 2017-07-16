class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  // defining a second constructor, calls the implicit primary constructor
  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
  private val g = gcd(x,y)
  def numer = x/g
  def denom = y/g

  def < (that: Rational) = numer * that.denom < that.numer * denom
  def max(that: Rational) = if(this < that) that else this

  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def unary_- : Rational = new Rational(-numer, denom)

  def - (that: Rational) = this + -that

  override def toString() = numer + "/" + denom
}



var x = new Rational(1,2)
x.numer
x.denom

var y = new Rational(2,3)
x + y

x = new Rational(1,3)
y = new Rational(5,7)
val z = new Rational(3,2)

x - y - z
y + y
x < y
x max y
new Rational(2)

val r1 = new Rational(465,45609)

r1 + r1 + r1 + r1 + r1
