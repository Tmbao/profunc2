package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
    val valA = a()
    val valB = b()
    val valC = c()
    valB * valB - 4 * valA * valC
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val valA = a()
    val valB = b()
    val valC = c()
    val valDelta = delta()

    if (valDelta < 0) Set()
    else if (valA == 0)
      if (valB == 0) Set()
      else Set(-valC / valB)
    else Set((-valB + math.sqrt(valDelta)) / (2 * valA), (-valB + math.sqrt(valDelta)) / (2 * valA))
  }
}
