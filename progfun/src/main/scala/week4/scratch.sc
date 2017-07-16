import week4.{List,Cons,Nil}

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])


def elemN(n: Int, l: week4.List[Any]): Any = {
  if(l.isEmpty || n < 0) throw new IndexOutOfBoundsException
  else if(n == 0) l.head
  else elemN(n-1, l.tail)
}

val l1 = singleton[Any](1)
val l2 = new Cons[Any](1, new Cons[Any](2, new Cons[Any](3, new Cons[Any]("end", new Nil()))))
elemN(0,l1)
elemN(0,l2)
elemN(1,l2)
elemN(2,l2)
elemN(3,l2)
//elemN(4,l2)

def nth[T](n: Int, xs: List[T]): T =
  if(xs.isEmpty) throw new IndexOutOfBoundsException
  else if(n == 0) xs.head
  else nth(n-1, xs.tail)

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
nth(2, list)
//nth(-1,list)


// This is the example where a method is used in a place where a function type
// is expected, and it is converted automatically to the anonymous function form.
// It was not clear at all that this is what he was after.
// eta expansion!!!
week4.List(1,2)
week4.List()
week4.List(1)

