package week4

import java.util.NoSuchElementException

/**
  * Created by bpafoshizle on 1/21/17.
  */
trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}


/*
Field definitions in classes are really special cases of methods. And they can override methods, and they can implement
abstract methods and traits. The difference between the val and the def concerns only the initialization. A val is
evaluated when the object is first initialized. Where, whereas a def is evaluated each time it is referenced.
 */
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false //Cons cells are never empty.
}
object Nil extends List[Nothing] {
    def isEmpty = true
    // Return type of NoSuchElementException is Nothing, and we are writing return type of head and tail below explicitly.
    // Nothing is a subtype of any other type, including T.
    def head: Nothing = throw new NoSuchElementException("Nil.head")
    def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other

  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if(x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if(x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem

  override def toString = "{" + left + elem + right + "}"
}

object test {
  val x: List[String] = Nil
  def f(xs: List[NonEmpty]) = xs prepend Empty
}

//class Nil[T] extends List[T] {
//  def isEmpty = true
//  // Return type of NoSuchElementException is Nothing, and we are writing return type of head and tail below explicitly.
//  // Nothing is a subtype of any other type, including T.
//  def head: Nothing = throw new NoSuchElementException("Nil.head")
//  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
//}

object List {
  def apply() = {
    Nil
  }
  def apply(x: Int) = {
    new Cons(x, Nil)
  }
  def apply(x: Int, y: Int) = {
    new Cons(x, new Cons(y, Nil))
  }
}


