def insert(x: Int, xs: List[Int]): List[Int] = xs match {
   case List() => List(x)
   case y :: ys => if(x < y) x :: xs else insert(x, ys)
}

def init[T](xs: List[T]): List[T] = xs match {
   case List() => throw new Error("init of an empty list")
   case List(x) => List()
   case y :: ys => y :: init(ys)
}

init(List(1,2,3))
//init(List())
init(List(1))
init(List("abc","def","xyz"))

def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
   case List() => xs
   case y :: ys => if(n == 0) ys else y :: removeAt(n-1, ys)
}

removeAt(1, List('a','b','c','d'))

def flatten(xs: List[Any]): List[Any] = xs match {
   case Nil => Nil
   case y :: ys => (y match {
      case z :: zs => flatten(z :: zs)
      case z => List(z)
   }) ::: flatten(ys)
}

flatten(List())
flatten(List(List(1,1),2,3,5,8))
flatten(List(List(1,1), 2, List(3,List(5, 8))))
flatten(List(List(1,1,List(1)), 2, List(3,List(5, 8))))


