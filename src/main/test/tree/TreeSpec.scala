package tree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TreeSpec extends AnyFlatSpec with Matchers {
  val simpleTree: BinaryTree[Int] = BinaryTree(
    Node(1, BinaryTree(Node(2, EmptyTree, EmptyTree)), BinaryTree(Node(3, EmptyTree, EmptyTree)))
  )

  val complexTree: BinaryTree[Int] = BinaryTree(
    Node(
      1,
      BinaryTree(Node(2, BinaryTree(Node(4, EmptyTree, EmptyTree)), BinaryTree(Node(5, EmptyTree, EmptyTree)))),
      BinaryTree(Node(3, BinaryTree(Node(6, EmptyTree, EmptyTree)), BinaryTree(Node(7, EmptyTree, EmptyTree))))
    )
  )

  // Tests for Size
  "A BinaryTree" should "calculate its size correctly" in {
    EmptyTree.size shouldBe 0
    simpleTree.size shouldBe 3
    complexTree.size shouldBe 7
  }

  // Tests for DFS Search
  "DFS Search" should "find values present in the tree" in {
    simpleTree.dfsSearch(_ == 2) shouldBe Some(2)
    complexTree.dfsSearch(_ == 7) shouldBe Some(7)
  }

  it should "return None for values not present in the tree" in {
    simpleTree.dfsSearch(_ == 10) shouldBe None
  }

  // Tests for Min and Max
  "A BinaryTree" should "find the minimum and maximum values" in {
    val testTree = BinaryTree(
      Node(
        5,
        BinaryTree(Node(3, BinaryTree(Node(1, EmptyTree, EmptyTree)), BinaryTree(Node(4, EmptyTree, EmptyTree)))),
        BinaryTree(Node(8, BinaryTree(Node(6, EmptyTree, EmptyTree)), BinaryTree(Node(9, EmptyTree, EmptyTree))))
      )
    )

    testTree.min shouldBe Some(1)
    testTree.max shouldBe Some(9)
  }

  it should "handle single element trees correctly for min and max" in {
    val singleElementTree = BinaryTree(Node(42, EmptyTree, EmptyTree))
    singleElementTree.min shouldBe Some(42)
    singleElementTree.max shouldBe Some(42)
  }

  // Tests for Add
  "add" should "correctly add elements to the tree" in {
    val tree = EmptyTree.add(1)
    tree shouldBe BinaryTree(Node(1, EmptyTree, EmptyTree))

    val updatedTree = tree.add(2).add(3)
    updatedTree shouldBe BinaryTree(
      Node(1, BinaryTree(Node(2, EmptyTree, EmptyTree)), BinaryTree(Node(3, EmptyTree, EmptyTree)))
    )
  }

  // Tests for Delete
  "delete" should "correctly remove elements from the tree" in {
    val tree = BinaryTree(Node(2, BinaryTree(Node(1, EmptyTree, EmptyTree)), BinaryTree(Node(3, EmptyTree, EmptyTree))))

    val updatedTree = tree.delete(3)
    updatedTree shouldBe Right(BinaryTree(Node(2, BinaryTree(Node(1, EmptyTree, EmptyTree)), EmptyTree)))
  }

  it should "handle deletion of non-existent elements gracefully" in {
    val tree = BinaryTree(Node(2, BinaryTree(Node(1, EmptyTree, EmptyTree)), BinaryTree(Node(3, EmptyTree, EmptyTree))))

    val updatedTree = tree.delete(4)
    updatedTree shouldBe Left(NoSuchValue)
  }

  // Tests for Custom Folds
  "A BinaryTree" should "correctly apply custom fold operations" in {
    val sum = complexTree.foldLeft(0)(_ + _)
    sum shouldBe 28

    val product = complexTree.foldLeft(1)(_ * _)
    product shouldBe 5040
  }

}
