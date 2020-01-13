

def times(chars: List[Char]): List[(Char, Int)] = {
  val map = chars.map(char => (char, 1))
  var lookUp: Map[Char, Int] = Map()
  map.map{ tuple =>
    val ch = tuple._1
    val count = lookUp.get(ch).getOrElse(0)
    if (count == 0) lookUp = lookUp + (ch -> 1)
    else lookUp = lookUp + (ch -> (count + 1))
  }
  lookUp.toList
}

val l = "abcabaasrtafawdawpeobnfcas".toCharArray.toList
val s = times(l)

s.head
s.sortBy(_._2)


var m: Map[Char, Int] = Map('a' -> 1)
m = m + ('a' -> 2)

m.get('c').getOrElse(0)

m.toList