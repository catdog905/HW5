package ventilation

object Ventilation {
  def firstSolution(degrees: List[Int], k: Int): List[Int] = degrees.view.sliding(k).map(lst => lst.max).toList

  def secondSolution(degrees: LazyList[Int], k: Int): Iterator[Int] = degrees.view.sliding(k).map(lst => lst.max)
}
