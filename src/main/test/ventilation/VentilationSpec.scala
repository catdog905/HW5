package ventilation

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VentilationSpec extends AnyFlatSpec with Matchers {
  "ilzida" should "succeed ventilators" in {
    Ventilation.firstSolution(List(1, 2, 3, 4), 2) shouldEqual List(2, 3, 4)
    Ventilation.firstSolution(List(1, 2, 3, 4), 4) shouldEqual List(4)
    Ventilation.firstSolution(List(1, 2), 4) shouldEqual List(2)

  }

  "ilzida" should "succeed ventilators with deadlines" in {
    Ventilation.secondSolution(LazyList.from(5), 1).take(5).toList shouldEqual List(5, 6, 7, 8, 9)
    Ventilation.secondSolution(LazyList.from(5), 3).take(5).toList shouldEqual List(7, 8, 9, 10, 11)
  }
}
