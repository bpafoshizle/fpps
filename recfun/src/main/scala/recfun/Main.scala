package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Parentheses Balancing")
    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))

    println("Counting Change")
    //println("countChange(0,List(1,2)): " + countChange(0,List(1,2)))
    //println("countChange(4,List()): " + countChange(4,List()))
    println("countChange(4,List(1,2)): " + countChange(4, List(1, 2)))
    //println("countChange(4,List(1,2,3)): " + countChange(4,List(1,2,3)))
    //println("countChange(4,List(2,1)): " + countChange(4, List(2,1)))
    //println("countChange(4, List(3,2,1): " + countChange(4, List(3,2,1)))

  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(openCount: Int, closeCount: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty)
        (openCount == 0 && closeCount == 0)
      else {
        if (chars.head == '(') balanceIter(openCount + 1, closeCount, chars.tail)
        else if (chars.head == ')') {
          if (openCount > 0) balanceIter(openCount - 1, closeCount, chars.tail)
          else balanceIter(openCount, closeCount + 1, chars.tail)
        }
        else balanceIter(openCount, closeCount, chars.tail)
      }
    }

    balanceIter(0, 0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else {
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
}
