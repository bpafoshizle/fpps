def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (b < a) acc
    else loop(a+1, f(a) + acc)
  }
  loop(a, 0)
}

def cube(x: Int) = x * x * x
def square(x: Int) = x * x
def ident(x: Int) = x

sum(x => x)(1,5)
sum(ident)(1,5)
sum(x => x*x)(3,5)
sum(square)(3,5)
sum(x => x*x*x)(3,5)
sum(cube)(3,5)

def product(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if(a > b) acc
    else loop(a+1, f(a) * acc)
  }
  loop(a, 1)
}

product(x => x)(1,1)
product(x => x)(1,2)
product(x => x)(1,3)
product(x => x)(0,1)
product(x => x)(2,2)
product(x => x*x)(3,4)


def fact(n: Int) = {
  product(x=>x)(1, n)
}

fact(1)
fact(2)
fact(3)
fact(4)
fact(5)


def mult(a: Int, b: Int) = a*b
def add(a: Int, b: Int) = a+b

def agg(f:Int => Int)(u: Int)(cf: (Int, Int) => Int)(a: Int,b: Int) = {
  def loop(a: Int, acc: Int): Int = {
    if(a > b) acc
    else loop(a+1, cf(f(a), acc))
  }
  loop(a, u)
}

agg(x => x)(1)(mult)(1,1)
agg(x => x)(1)(mult)(1,2)
agg(x => x)(1)(mult)(1,3)
agg(x => x)(1)(mult)(0,1)
agg(x => x)(1)(mult)(2,2)
agg(x => x*x)(1)(mult)(3,4)

agg(x => x)(0)(add)(1,5)
agg(ident)(0)(add)(1,5)
agg(x => x*x)(0)(add)(3,5)
agg(square)(0)(add)(3,5)
agg(x => x*x*x)(0)(add)(3,5)
agg(cube)(0)(add)(3,5)
