package recfun

import scala.collection.mutable.ListBuffer

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
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
    def balanced(chars: List[Char], openParen: Int): Boolean = {
      if (chars.isEmpty) {
        openParen == 0
      } else {
        val head = chars.head
        val tmpNumber = {
          if (head == '(') openParen + 1
          else if (head == ')') openParen - 1
          else openParen
        }
        if (tmpNumber >= 0) balanced(chars.tail, tmpNumber)
        else false
      }
    }
    balanced(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) if (money == 0) 1 else 0
    else if (money - coins.head == 0) 1
    else if (money - coins.head < 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
//    def getCount(coinValueList: List[(Int, Int)], count: Int): Int = {
//      if (coinValueList.isEmpty) {
//        count
//      } else {
//        val lst = ListBuffer[(Int, Int)]()
//        var newCount = count
//        for ((coin, total) <- coinValueList) {
//          if (total < money) {
//            for (c <- coins) {
//              if (c >= coin) {
//                val e = (c, total + c)
//                lst += e
//              }
//            }
//          } else if (total == money) {
//            newCount += 1
//          }
//        }
//        getCount(lst.toList, newCount)
//      }
//    }
//    val b = coins.map {c => (c, c)}
//    getCount(b, 0)
//  }
