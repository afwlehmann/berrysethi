package berrysethi


sealed abstract class BRTree
case class Concat(l: BRTree, r: BRTree) extends BRTree
case class Or(l: BRTree, r: BRTree) extends BRTree
case class Star(n: BRTree) extends BRTree
case class QuestionMark(n: BRTree) extends BRTree

class Leaf protected (val v: Char, val index: Int) extends BRTree {

  override def toString: String =
    "Leaf(" + v + "," + index + ")"

}

object Leaf {

  var lastLeafIndex = -1

  def apply(v: Char): Leaf = {
    lastLeafIndex += 1
    new Leaf(v, lastLeafIndex)
  }

  def unapply(l: Leaf): Option[(Char, Int)] =
    Some((l.v, l.index))

}
