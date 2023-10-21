package collections

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CollectionsSpec extends AnyFlatSpec with Matchers {
  "findGaps" should "find all gaps that have gaps between them or return None" in {
    Collections.findGaps(Seq(1, 2, 3, 4)) shouldEqual None
    Collections.findGaps(Seq(1, 2, 8)) shouldEqual Some(Seq((2, 8)))
    Collections.findGaps(Seq(3, 5, 7)) shouldEqual Some(Seq((3, 5), (5, 7)))
  }

  "minFold" should "return key-value pair with the minimum value in the map or return None" in {
    Collections.minFold(Map("one" -> 1, "two" -> 2, "three" -> 3)) shouldEqual Some(("one", 1))
    Collections.minFold(Map()) shouldEqual None
  }

  "minReduce" should "return key-value pair with the minimum value in the map or return None" in {
    Collections.minReduce(Map("one" -> 1, "two" -> 2, "three" -> 3)) shouldEqual Some(("one", 1))
    Collections.minReduce(Map()) shouldEqual None
  }

  "minRecursion" should "return key-value pair with the minimum value in the map or return None" in {
    Collections.minRecursion(Map("one" -> 1, "two" -> 2, "three" -> 3)) shouldEqual Some(("one", 1))
    Collections.minRecursion(Map()) shouldEqual None
  }

  "scanLeft" should "return the same result as scanLeft of the standard library" in {
    Collections.scanLeft(42)(1 :: 2 :: 3 :: 4 :: Nil)(_ * _) shouldEqual (1 :: 2 :: 3 :: 4 :: Nil).scanLeft(42)(_ * _)
    Collections.scanLeft(42)(List[Int]())(_ * _) shouldEqual List[Int]().scanLeft(42)(_ * _)
  }

  "count" should "return number of consistent occurrences of each character in the string" in {
    Collections.count("Hello  Woooorld") shouldEqual
      List(('H', 1), ('e', 1), ('l', 2), ('o', 1), (' ', 2), ('W', 1), ('o', 4), ('r', 1), ('l', 1), ('d', 1))
  }
}
