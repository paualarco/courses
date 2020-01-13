import java.util

val emptyArr = new Array[String](0)
"".isEmpty
val nemptyArr = Array(1,2,3)



def getNthElement[T](n: Int, l:List[T]): T = {
  if (l.isEmpty) throw new IndexOutOfBoundsException
  if(n==0) l.head
  else getNthElement(n - 1, l.tail)
}


