def squareList(xs: List[Int]): List[Int] = xs match {
   case Nil => xs
   case y :: ys => (y*y) :: squareList(ys)
}

def squareListMap(xs: List[Int]): List[Int] =
   xs map (x => x*x)

val nums = List(2, 3, 5, 1, 0, 22, 9, 10, 48, 101, 42, 54, 81, -4, -1)
val fruits = List("apple", "pineapple", "orange", "banana")

squareList(nums)
squareListMap(nums)

nums filter (x => x > 0)
nums filterNot (x => x > 0)
nums partition (x => x > 0)


nums takeWhile (x => x > 0)
nums dropWhile (x => x > 0)
nums span (x => x > 0)


def pack[T](xs: List[T])(eq: (T,T) => Boolean): List[List[T]] = xs match {
   case Nil => Nil
   case x :: xs1 =>
      val (left, right) = xs.span(eq(_,x))
      left :: pack(right)(eq)
}

def encode[T](xs: List[T])(eq: (T,T) => Boolean): List[(T,Int)] = {
   val packed = pack(xs)(eq)
   packed map (x => (x.head, x.length))
}

pack(List("a", "a", "a", "b", "c", "c", "a"))((x: String, y: String) => x.equals(y))
encode(List("a", "a", "a", "b", "c", "c", "a"))((x: String, y: String) => x.equals(y))