package ADT

sealed  trait MyBinaryTree[+T] {
  def isEmpty: Boolean
  def size: Int
  def printElements: String
  override def toString: String = "[" + printElements + "]"
}

case object EmptyBinaryTree extends  MyBinaryTree[Nothing] {
  override def isEmpty: Boolean = true
  override def printElements: String = ""
  override def size: Int = 0
}

case class Branch[T](left: MyBinaryTree[T], right: MyBinaryTree[T]) extends  MyBinaryTree[T] {
  override def isEmpty: Boolean = false
  override def printElements: String =
    if(isEmpty) ""
    else left + " " + right
  override def size: Int = 1 + left.size + right.size
}

case class Leaf[T](value: T) extends MyBinaryTree[T] {
  override def isEmpty: Boolean = false
  override def printElements: String = value.toString
  override def size: Int = 1
}

object MyBinaryTree {
  def main(args: Array[String]): Unit = {
    val tree1: Branch[Int] = Branch(Branch(Leaf(5), Branch(Leaf(3), Leaf(2))), Branch(Leaf(8), Leaf(9)))
    val tree1Size = tree1.size
    println(tree1.toString)
    println(tree1Size)
  }
}
