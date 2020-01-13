import scala.Char
import scala.collection.mutable.ArrayBuffer


val is = isParenthesesBalanced(0,"(.,())".toCharArray.toList)

val isNot0 = isParenthesesBalanced(0,"(.,()".toCharArray.toList)
val isNot1 = isParenthesesBalanced(0,").,()".toCharArray.toList)


