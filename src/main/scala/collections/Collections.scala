package collections

import scala.annotation.tailrec

object Collections {

  /*
    In a sorted list find all pairs of two neighbor numbers which have a gap between them
    None for Seq(1, 2, 3, 4)
    Some(Seq((2, 8))) for Seq(1, 2, 8)
    Some(Seq((3, 5), (5, 7))) for Seq(3, 5, 7)
   */
  def findGaps(l: List[Int]): Option[List[(Int, Int)]] = {
    @tailrec
    def findGapsTail(l: List[Int], acc: List[(Int, Int)]): List[(Int, Int)] = l match {
      case Nil                            => acc
      case _ :: Nil                       => acc
      case x1 :: x2 :: xs if x2 != x1 + 1 => findGapsTail(x2 :: xs, acc :+ (x1, x2))
      case _ :: x2 :: xs                  => findGapsTail(x2 :: xs, acc)
    }

    val result = findGapsTail(l, List())
    if (result.isEmpty)
      None
    else
      Some(result)
  }

  /*
    Find key-value pair with the minimum value in the map
    try to implement min in different ways (fold, reduce, recursion)
   */
  def minFold(map: Map[String, Int]): Option[(String, Int)] = map.size match {
    case 0 => None
    case _ =>
      Some(map.fold(map.head)({ case ((curKey, curValue), (accKey, accValue)) =>
        if (curValue < accValue) (curKey, curValue) else (accKey, accValue)
      }))
  }

  def minReduce(map: Map[String, Int]): Option[(String, Int)] = map.size match {
    case 0 => None
    case _ =>
      Some(map.reduce[(String, Int)]({ case ((curKey, curValue), (accKey, accValue)) =>
        if (curValue < accValue) (curKey, curValue) else (accKey, accValue)
      }))
  }

  def minRecursion(map: Map[String, Int]): Option[(String, Int)] = {
    @tailrec
    def minTailRec(list: List[(String, Int)], acc: (String, Int)): (String, Int) = list match {
      case Nil => acc
      case (curKey: String, curValue: Int) :: xs =>
        val (accKey, accValue) = acc
        val newAcc = if (curValue < accValue) (curKey, curValue) else (accKey, accValue)
        minTailRec(xs, newAcc)
    }

    if (map.isEmpty)
      None
    else
      Some(minTailRec(map.toList, map.head))
  }

  // Implement scanLeft - running total, applying [f] to elements of [list] (not using scans ofc)
  def scanLeft[T](zero: T)(list: Seq[T])(f: (T, T) => T): Seq[T] = {
    @tailrec
    def scanLeftInner(lst: List[T], result: List[T]): List[T] = (lst.headOption, result.headOption) match {
      case (None, _)               => result
      case (Some(elem), None)      => scanLeftInner(lst.tail, elem :: Nil)
      case (Some(elem), Some(cum)) => scanLeftInner(lst.tail, f(cum, elem) :: result)
    }
    scanLeftInner(zero :: list.toList, Nil).reverse
  }

  // Count the consistent occurrences of each character in the string
  def count(s: String): List[(Char, Int)] = {
    @tailrec
    def consistentOccurrences(lst: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] =
      (lst.headOption, acc.headOption) match {
        case (None, _)          => acc
        case (Some(head), None) => consistentOccurrences(lst.tail, (head, 1) :: Nil)
        case (Some(head), Some((accHead, count))) if head == accHead =>
          consistentOccurrences(lst.tail, (accHead, count + 1) :: acc.tail)
        case (Some(head), Some((_, _))) =>
          consistentOccurrences(lst.tail, (head, 1) :: acc)
      }

    consistentOccurrences(s.toList, Nil).reverse
  }
}
