
import common._
import patmat.Huffman.{CodeTree, _}

object Huffman {

  abstract class CodeTree {val weight: Int}
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], override val weight: Int) extends CodeTree
  case class Leaf(char: Char, override val weight: Int) extends CodeTree


  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Fork(_, _, _, weight) => weight
    case Leaf(_, weight) => weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_, _, chars, _) => chars
    case Leaf(char, _) => List(char)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


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

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs.sortBy(_._2).map(t => new Leaf(t._1, t._2))

  def string2Chars(str: String): List[Char] = str.toList

  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

  def combine(trees: List[CodeTree]): List[CodeTree] = {
    if (trees.length == 1) trees
    else {
      val leftTree = trees.head
      val rightTree = trees.tail.head
      val charsList: List[Char] = chars(leftTree) ::: chars(rightTree)
      val totalWeight = leftTree.weight + rightTree.weight
      val fork = new Fork(leftTree, rightTree, charsList, totalWeight)
      val newList = fork :: trees.tail.tail
      newList.sortBy(_.weight)
    }
  }

  def until(singleton: List[CodeTree] => Boolean, combine: List[CodeTree] => List[CodeTree])(tree: List[CodeTree]): List[CodeTree] = {
    def iterate(t: List[CodeTree]): List[CodeTree] = {
      if(singleton(tree)) tree
      else iterate(combine(tree))

    iterate(tree)
  }

  def createCodeTree(chars: List[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  }
}



val chars = Huffman.string2Chars("ABDEFGAAWERFAAW")

//val sorted = Huffman.makeOrderedLeafList(Huffman.times(chars))

//val combined = Huffman.combine(sorted)


val tree = Huffman.createCodeTree(chars)



/*if (trees.length <= 2) trees
  else {
    val leftTree = trees.head
    val rightTree = trees.tail.head
    val charsList: List[Char] = chars(leftTree) ::: chars(rightTree)
    val totalWeight = leftTree.weight + rightTree.weight
    val fork = new Fork(leftTree, rightTree, charsList, totalWeight)
    val newList = fork :: trees.tail.tail
    newList.sortBy(_.weight)
  }*/