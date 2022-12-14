import E106.TreeNode

import java.util.concurrent.atomic.AtomicInteger

object E1339 {

  def sumTree(node: TreeNode, cumSum: AtomicInteger): Unit = {
    cumSum.addAndGet(node.value)
    if (node.left != null) sumTree(node.left, cumSum)
    if (node.right != null) sumTree(node.right, cumSum)
  }

  def cumSumTree(node: TreeNode): Unit = {
    var newValue = node.value
    if (node.left != null) {
      cumSumTree(node.left)
      newValue += node.left.value
    }
    if (node.right != null) {
      cumSumTree(node.right)
      newValue += node.right.value
    }
    node.value = newValue
  }

  def getMinDiff(node: TreeNode, sum: Int, diff: AtomicInteger): Unit = {
    if (Math.abs(sum - 2 * node.value) < Math.abs(sum - 2 * diff.get())) diff.set(node.value)
    if (node.left != null) getMinDiff(node.left, sum, diff)
    if (node.right != null) getMinDiff(node.right, sum, diff)
  }

  def maxProduct(root: TreeNode): Int = {
    val sum = new AtomicInteger()
    sumTree(root, sum)
    cumSumTree(root)
    val minDiff = new AtomicInteger()
    getMinDiff(root, sum.get(), minDiff)
    (BigInt.int2bigInt(minDiff.get()) * BigInt.int2bigInt(sum.get() - minDiff.get()) % 1000000007).intValue
  }

  def main(args: Array[String]): Unit = {
  }

}
