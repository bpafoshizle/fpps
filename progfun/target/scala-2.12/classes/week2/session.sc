object session {
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if(a > b) acc
      else loop(a+1, acc + f(a))
    }
    loop(a, 0)
  }

  sum((x: Int) => x)(1, 2)
  sum((x: Int) => x)(1, 4)
  sum(x => x * x)(3, 5)



  /* Lecture 2.2 */
  def id(id: Int): Int = {id}
  def sqr(x: Int): Int = {x*x}
  def cube(x: Int): Int = {x*x*x}

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    // 1 is "unit value" for multiplication
    if(a > b) 1 else f(a) * product(f)(a+1,b)
  }

  def sum1(f: Int => Int)(a: Int, b:Int): Int = {
    // 0 is "unit value" of addition
    if(a > b) 0 else f(a) * sum(f)(a+1,b)
  }



  def factorial(x: Int): Int = {
    product(x => x)(1,x)
  }



  def aggregate(f: (Int => Int) => (Int, Int) => Int,
                g: (Int) => Int): (Int,Int) => Int = {
    f(g)
  }

  def mapReduce(f: Int => Int,
                combine: (Int, Int) => Int,
                zero: Int)(a: Int, b: Int): Int = {
    if(a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+1,b))
  }


  product(id)(1,3)
  product(id)(1,4)
  product(id)(1,5)
  product(x => x * x)(3,7)
  product(x => x * x)(3,4)

  factorial(3)
  factorial(4)
  factorial(5)

  aggregate(product,id)(1,3)
  aggregate(product,id)(1,4)
  aggregate(product,id)(1,5)

  sum1(id)(1,3)
  sum1(id)(1,4)
  sum1(id)(1,5)

  aggregate(sum1,id)(1,3)
  aggregate(sum1,id)(1,4)
  aggregate(sum1,id)(1,5)

  def productMR(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x,y) => x * y, 1)(a,b)

  productMR(id)(1,3)
  productMR(id)(1,4)
  productMR(id)(1,5)
  productMR(x => x * x)(3,7)
  productMR(x => x * x)(3,4)

}