object session {
  def abs(x:Double) = if (x < 0) -x else x


  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double, x: Double) = {
      abs(x - (guess * guess))/x < .001
    }

    def improve(guess: Double) =(guess + x / guess) / 2

    sqrtIter(1.0)
  }

  sqrt(2)
  sqrt(4)
  sqrt(1e-6)
  sqrt(1e60)


  val x = 0
  def f(y: Int) = y + 1
  val result = {
    val x = f(3);
    x * x
  } + x

  def factorial(n: Int): Int = {
    def factorialIter(prev: Int, n: Int): Int = {
      if(n == 0) prev else factorialIter(prev * n, n-1)
    }
    factorialIter(1, n)
  }

  factorial(2)
  factorial(3)
  factorial(4)
  factorial(5)
  factorial(10)
}
