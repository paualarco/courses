import forcomp.Anagrams.{Occurrences, Word}

def wordOccurrences(w: Word): Occurrences = w
  .toLowerCase()
  .filter(_ != ' ')
  .groupBy(word => word)
  .map( t => (t._1, t._2.length))
  .toList
  .sortBy(_._1)

type Occurrences = List[(Char, Int)]

val occ: Occurrences = List(('1', 1))


def occurrencesToString(occurrences: Occurrences): String = {
  occurrences.flatMap(t=> for( i <- 1 to t._2 ) yield t._1 ).mkString
}

val occurrences = wordOccurrences("abab")

occurrencesToString(occurrences)

def combinations(occurrences: Occurrences): List[Occurrences] = {

  def sequences(occStr: String, acc: List[String]): List[String] = {
    occStr match {
      case "" => acc
      case _ => {
        val curr = acc.map(_ + occStr.head)
        sequences(occStr.tail, acc ::: curr)
      }
    }
  }
  val occStrList = sequences(occurrencesToString(occurrences), List(""))
  println("Occurrences string list")
  occStrList.foreach(println)
  occStrList.map(wordOccurrences(_)).toSet.toList
}


combinations(wordOccurrences("aabb"))


val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
val y = List(('r', 1))
val xm = x.toMap
def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  val xm = x.toMap
  val ym = y.toMap
  def substractChar(occ: (Char, Int)): (Char, Int) = {
    val (char, freq) = occ
    (char, xm.getOrElse(char, 0) - freq)
  }
  val xmUpdated = xm ++ (ym map substractChar)
  xmUpdated.toList.filter(_._2 > 0)
}

val xSubstracted = subtract(x, y)