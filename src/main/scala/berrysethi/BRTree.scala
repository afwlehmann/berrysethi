/**
 * BRTree.scala
 * @author Alexander Lehmann <lehmanna@in.tum.de>
 */


package berrysethi


protected sealed abstract class BRTree
protected case class Concat(l: BRTree, r: BRTree) extends BRTree
protected case class Or(l: BRTree, r: BRTree) extends BRTree
protected case class Asterisk(n: BRTree) extends BRTree
protected case class QuestionMark(n: BRTree) extends BRTree


/*
 * Scala's case classes automatically override AnyRef's equals and hashCode
 * methodes so as to provide structural equality checks.
 * Since we want to be able to distinguish between all instances of Leaf,
 * we thus cannot use a case class in this place. Instead, we define the class
 * and companion ourselves.
 */

protected class Leaf(val v: Char) extends BRTree {

  override def toString: String =
    "Leaf(" + v + ")"

}

protected object Leaf {

  def apply(v: Char): Leaf =
    new Leaf(v)


  def unapply(l: Leaf): Option[Char] =
    Some(l.v)

}
