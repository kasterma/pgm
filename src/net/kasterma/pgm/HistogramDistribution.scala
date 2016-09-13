package net.kasterma.pgm

/**
  * Created by kasterma on 13/09/16.
  */
class DistributionError(message: String) extends Exception(message)

object HistogramDistribution {
  def apply[T](vals: List[T], probs: List[Double]): HistogramDistribution[T] =
    new HistogramDistribution(vals, probs, "(" + (vals zip probs mkString ", ") + ")")

  def apply[T](vals: List[T]): HistogramDistribution[T] =
    new HistogramDistribution(vals, (1 to vals.length) map (x => 1.0/vals.length) toList, "Unif: " + vals.mkString(", ") + ".")
}

class HistogramDistribution[T] (val vals: List[T], val probs: List[Double], val name: String) {
  if (vals.length != vals.distinct.length) {
    throw new DistributionError("HistogramDistribution: vals not all distinct")
  }

  if (vals.length != probs.length) {
    throw new DistributionError("HistogramDistribution: length of vals and probs distinct")
  }

  if (Math.abs(probs.sum - 1.0) > 0.001) {
    throw new DistributionError("HistogramDistribution: probs.sum too far from 1.0.")
  }

  override def toString: String = name
}
