package tree

import scala.annotation.tailrec
import scala.collection.mutable

case class Node[+T](value: T, left: Tree[T], right: Tree[T])

case object Node {
  def apply[T](value: T, left: Tree[T], right: Tree[T]) = new Node(value, left, right)

  def apply[T](value: T): Node[T] = new Node(value, EmptyTree, EmptyTree)
  def apply[T](value: T, left: Node[T], right: Node[T]) = new Node(value, Tree(left), Tree(right))

  def apply[T](value: T, left: T, right: T) = new Node(value, Tree(left), Tree(right))
}

sealed trait Tree[+T] {
  def print(): Unit
  def size: Int
}

case object EmptyTree extends Tree[Nothing] {
  override def print(): Unit = Predef.print("EmptyTree")

  def add[T](value: T): BinaryTree[T] = Tree(Node(value))

  override def size: Int = 0
}

sealed trait DeletionError
case object NoSuchValue extends DeletionError

case object InternalError extends DeletionError

case class BinaryTree[T](root: Node[T]) extends Tree[T] {
  def add(value: T): BinaryTree[T] = {
    def changeFirstCondElem(
      elements: List[Node[T]],
      check: Node[T] => Boolean,
      transform: Node[T] => Node[T]
    ): List[Node[T]] = {
      @tailrec
      def changeHelper(
        elements: List[Node[T]],
        check: Node[T] => Boolean,
        transform: Node[T] => Node[T],
        acc: List[Node[T]]
      ): List[Node[T]] = {
        if (check(elements.head))
          (acc.reverse :+ transform(elements.head)) ::: elements.tail
        else
          changeHelper(elements.tail, check, transform, elements.head :: acc)
      }

      changeHelper(elements, check, transform, List())
    }

    def buildNewTree(layers: List[List[Node[T]]]): BinaryTree[T] = {
      @tailrec
      def buildRec(curLayer: List[Node[T]], layers: List[List[Node[T]]]): List[Node[T]] =
        if (curLayer.length == 1)
          curLayer
        else
          buildRec(
            curLayer
              .grouped(2)
              .zip(layers.head)
              .collect({ case (left :: right :: Nil, Node(value, _, _)) =>
                Node(value, left, right)
              })
              .toList,
            layers.tail
          )
      Tree(buildRec(layers.head, layers.tail).head)
    }

    @tailrec
    def addRec(nodes: List[Node[T]], layers: List[List[Node[T]]]): BinaryTree[T] = {
      val ifNoneExist = (node: Node[T]) =>
        (node.left, node.right) match {
          case (EmptyTree, _) | (_, EmptyTree) => true
          case _                               => false
        }
      if (nodes.exists(ifNoneExist)) {
        val lastChangedLayer = changeFirstCondElem(
          nodes,
          ifNoneExist,
          {
            case Node(nodeValue, EmptyTree, right) => Node(nodeValue, Tree(value), right)
            case Node(nodeValue, left, EmptyTree)  => Node(nodeValue, left, Tree(value))
            case node                              => node
          }
        )
        buildNewTree(lastChangedLayer :: layers)
      } else
        addRec(
          nodes.flatMap({
            case Node(_, EmptyTree, EmptyTree)                      => List()
            case Node(_, EmptyTree, right: BinaryTree[T])           => List(right.root)
            case Node(_, left: BinaryTree[T], EmptyTree)            => List(left.root)
            case Node(_, left: BinaryTree[T], right: BinaryTree[T]) => List(left.root, right.root)
          }),
          nodes :: layers
        )
    }

    addRec(List(root), List())
  }

  def delete(value: T): Either[DeletionError, Tree[T]] = {

    @tailrec
    def getDeepestValue(q: mutable.Queue[Node[T]], ancestors: Map[Node[T], Node[T]]): (T, List[Node[T]]) = {
      @tailrec
      def buildBackPath(path: List[Node[T]], node: Option[Node[T]]): List[Node[T]] = node match {
        case Some(node) => buildBackPath(node :: path, ancestors.get(node))
        case None       => path
      }

      val curNode = q.dequeue()
      val Node(curValue, curLeft, curRight) = curNode
      val newAncestors = (curLeft, curRight) match {
        case (node1: BinaryTree[T], node2: BinaryTree[T]) =>
          q.enqueue(node1.root)
          q.enqueue(node2.root)
          ancestors ++ Map(node1.root -> curNode, node2.root -> curNode)
        case (node: BinaryTree[T], _) =>
          q.enqueue(node.root)
          ancestors + (node.root -> curNode)
        case (_, node: BinaryTree[T]) =>
          q.enqueue(node.root)
          ancestors + (node.root -> curNode)
        case _ => ancestors
      }

      if (q.nonEmpty)
        getDeepestValue(q, newAncestors)
      else
        (curValue, buildBackPath(List(), Some(curNode)))
    }

    val (deepestValue, pathToDeepsetNode) = getDeepestValue(mutable.Queue[Node[T]](root), Map())

    def replaceWithDeepest(rootNode: Node[T]): (BinaryTree[T], Boolean) = {
      val Node(nodeValue, left, right) = rootNode
      if (nodeValue == value)
        (Tree(Node(deepestValue, left, right)), true)
      else
        (left, right) match {
          case (BinaryTree(root1), BinaryTree(root2)) =>
            val ((branch1, found1), (branch2, found2)) = (replaceWithDeepest(root1), replaceWithDeepest(root2))
            (Tree(Node(nodeValue, branch1, branch2)), found1 || found2)
          case (BinaryTree(root), EmptyTree) =>
            val (branch, found) = replaceWithDeepest(root)
            (Tree(Node(nodeValue, branch, EmptyTree)), found)
          case (EmptyTree, BinaryTree(root)) =>
            val (branch, found) = replaceWithDeepest(root)
            (Tree(Node(nodeValue, EmptyTree, branch)), found)
          case (EmptyTree, EmptyTree) =>
            (Tree(rootNode), false)
        }
    }
    def removeDeepestValue(curNode: Node[T], pathWithoutHead: List[Node[T]]): Either[DeletionError, Tree[T]] = {
      if (pathWithoutHead.isEmpty)
        Right(EmptyTree)
      else
        (pathWithoutHead.head, curNode.left, curNode.right) match {
          case (headNode, BinaryTree(left), right) if left == headNode =>
            removeDeepestValue(left, pathWithoutHead.tail) match {
              case Right(nextBranch) => Right(Tree(Node(curNode.value, nextBranch, right)))
              case Left(error)       => Left(error)
            }
          case (headNode, left, BinaryTree(right)) if right == headNode =>
            removeDeepestValue(right, pathWithoutHead.tail) match {
              case Right(nextBranch) => Right(Tree(Node(curNode.value, left, nextBranch)))
              case Left(error)       => Left(error)
            }
          case _ => Left(InternalError)
        }
    }
    if (value == deepestValue)
      removeDeepestValue(root, pathToDeepsetNode.tail)
    else
      removeDeepestValue(root, pathToDeepsetNode.tail) match {
        case Right(BinaryTree(node)) =>
          replaceWithDeepest(node) match {
            case (_, false)   => Left(NoSuchValue)
            case (tree, true) => Right(tree)
          }
        case Right(EmptyTree) => Right(EmptyTree)
        case Left(error)      => Left(error)
      }
  }

  def foldLeft[B](z: B)(f: (B, T) => B): B = {
    def foldLeftRec(node: Tree[T], acc: B): B = node match {
      case BinaryTree(Node(value, left, right)) =>
        val leftAcc = foldLeftRec(left, acc)
        val nodeAcc = f(leftAcc, value)
        foldLeftRec(right, nodeAcc)
      case EmptyTree => acc
    }

    foldLeftRec(this, z)
  }

  def min(implicit ord: Ordering[T]): Option[T] = {
    foldLeft[Option[T]](None) { (acc, value) =>
      acc match {
        case Some(minValue) => Some(ord.min(minValue, value))
        case None           => Some(value)
      }
    }
  }

  def max(implicit ord: Ordering[T]): Option[T] = {
    foldLeft[Option[T]](None) { (acc, value) =>
      acc match {
        case Some(maxValue) => Some(ord.max(maxValue, value))
        case None           => Some(value)
      }
    }
  }

  def dfsSearch(predicate: T => Boolean): Option[T] = {
    def dfsRec(node: Tree[T]): Option[T] = node match {
      case BinaryTree(Node(value, left, right)) =>
        if (predicate(value)) Some(value)
        else dfsRec(left) orElse dfsRec(right)
      case EmptyTree => None
    }

    dfsRec(this)
  }

  override def print(): Unit = {
    @tailrec
    def printRec[B <: T](nodes: List[Node[B]], acc: String): String = nodes match {
      case Nil => acc
      case _ =>
        printRec(
          nodes
            .flatMap(node => List(node.left, node.right))
            .collect({ case BinaryTree(root) => root }),
          acc + nodes.map(_.value).mkString("", " ", "\n")
        )
    }
    Predef.print(printRec(List(root), ""))
  }
  override def size: Int = 1 + root.left.size + root.right.size
}

case object Tree {
  def apply[T](root: Node[T]): BinaryTree[T] = BinaryTree(root)

  def apply[T](value: T): BinaryTree[T] = BinaryTree(Node(value))
}
