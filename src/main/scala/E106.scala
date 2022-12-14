

object E106 {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right

    def canEqual(other: Any): Boolean = other.isInstanceOf[TreeNode]

    override def equals(other: Any): Boolean = other match {
      case that: TreeNode =>
        (that canEqual this) &&
          value == that.value &&
          left == that.left &&
          right == that.right
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(value, left, right)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  def buildTree(inorder: Array[Int], postorder: Array[Int]): TreeNode = {
    buildTree(inorder.toList, postorder.toList)
  }

  def buildTree(inorder: List[Int], postorder: List[Int]): TreeNode =
    if (inorder.isEmpty) null
    else {
      val rootVal = postorder.last
      val inorderIndex = inorder.indexOf(rootVal)
      val (left, right) = inorder.splitAt(inorderIndex)
      val tree = new TreeNode(rootVal)
      tree.left = buildTree(left, postorder.take(left.size))
      tree.right = buildTree(right.tail, postorder.drop(left.size).init)
      tree
    }


  def main(args: Array[String]): Unit = {
    val firstTree = new TreeNode(3)
    firstTree.left = new TreeNode(9)
    firstTree.right = new TreeNode(20)
    firstTree.right.left = new TreeNode(15)
    firstTree.right.right = new TreeNode(7)
    println(buildTree(Array(9, 3, 15, 20, 7), Array(9, 15, 7, 20, 3)) == firstTree)
    val secondTree = new TreeNode(1)
    secondTree.left = new TreeNode(2)
    secondTree.right = new TreeNode(3)
    secondTree.left.left = new TreeNode(4)
    secondTree.right.left = new TreeNode(5)
    secondTree.right.right = new TreeNode(6)
    secondTree.right.right.left = new TreeNode(7)
    println(buildTree(Array(4, 2, 1, 5, 3, 7, 6), Array(4, 2, 5, 7, 6, 3, 1)) == secondTree)
    println(buildTree(Array(-1), Array(-1)) == new TreeNode(-1))
  }

}
