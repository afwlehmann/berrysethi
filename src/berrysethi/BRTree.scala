/**
 * BRTree.scala
 * @author Alexander Lehmann <lehmanna@in.tum.de>
 */


package berrysethi


sealed abstract class BRTree
case class Concat(l: BRTree, r: BRTree) extends BRTree
case class Or(l: BRTree, r: BRTree) extends BRTree
case class Asterisk(n: BRTree) extends BRTree
case class QuestionMark(n: BRTree) extends BRTree

//case class Leaf(v: Char) extends BRTree {
//  override def equals(that: Any): Boolean =
//    eq(that.asInstanceOf[AnyRef])
//}

class Leaf protected (val v: Char, val index: Int) extends BRTree {
  override def toString: String =
    "Leaf(" + v + "," + index + ")"

}

object Leaf {

  var lastIndex = -1


  def apply(v: Char): Leaf = {
    lastIndex += 1
    new Leaf(v, lastIndex)
  }


  def unapply(l: Leaf): Option[Char] =
    Some(l.v)

}
