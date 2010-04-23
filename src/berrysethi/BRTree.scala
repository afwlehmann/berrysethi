package berrysethi


sealed abstract class BRTree
case class Concat(l: BRTree, r: BRTree) extends BRTree
case class Or(l: BRTree, r: BRTree) extends BRTree
case class Asterisk(n: BRTree) extends BRTree
case class QuestionMark(n: BRTree) extends BRTree
case class Leaf(v: Char) extends BRTree
