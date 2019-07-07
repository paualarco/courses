package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      def genNextSeq(lastSeq: List[Int]): List[Int]  = {
        var counter = 0
        var current = List(1)
        for (i <- 0 until lastSeq.length - 1){
          current = current.::(lastSeq(i)+lastSeq(i+1))
        }
        current.::(1)
      }
      def genPascal(n:Int): List[Int] ={
        n match {
          case _ if n < 0 => List(0)
          case 0 => List(1)
          case _ => genNextSeq(genPascal(n-1))
        }
      }
      genPascal(r)(c)
    }


  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      val `(`:Char = "(".toCharArray.toList.head
      val `)`:Char = ")".toCharArray.toList.head
      def isParenthesesBalanced(incompleteParentheses: Int,
                                chrs: List[Char]): Boolean ={
        val isBalanced = incompleteParentheses==0
        if (chrs.isEmpty) return isBalanced
        if (isBalanced) {
          chrs.head match {
            case `(` => isParenthesesBalanced(1,chrs.tail)
            case `)` => return false
            case _ => isParenthesesBalanced(0,chrs.tail)
          }
        }
        else {
          chrs.head match {
            case `(` => isParenthesesBalanced(incompleteParentheses+1,chrs.tail)
            case `)` => isParenthesesBalanced(incompleteParentheses-1,chrs.tail)
            case _ => isParenthesesBalanced(incompleteParentheses,chrs.tail)
          }
        }
      }
      isParenthesesBalanced(0,chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
       money match {
        case 0 => 1
        case _ if (coins.isEmpty || money < 0) => 0
        case _ => countChange(money, coins.tail) + countChange(money-coins.head, coins)
      }
    }
}

