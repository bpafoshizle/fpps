package week4

/**
  * Created by bpafoshizle on 1/29/17.
  */
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Succ = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat{
  def isZero = true
  def predecessor: Nothing = throw new NoSuchElementException("Zero.predecessor"))
  def +(that: Nat): Nat = that
  def -(that: Nat): Nat = if(that.isZero) this else throw new NoSuchElementException("Zero - NonZero")
}

class Succ(n: Nat) extends Nat{
  def isZero = false
  def predecessor = n
  //def +(that: Nat): Nat = if(that.isZero) this else this.successor + that.predecessor
  def +(that: Nat): Nat = new Succ(n+that)
  def -(that: Nat): Nat = if(that.isZero) this else this.predecessor - that.predecessor
}


