package net.kasterma.pgm

/**
  * Created by kasterma on 12/10/16.
  */
object geneticExamples {

  case class indexedGene(index: Int, first: Int, second: Int) {
    def this(x: (Int, Int, Int)) = this(x._1, x._2, x._3)
  }

  class indexedGenotypeIterator(n: Int) extends Iterator[indexedGene] {
    val igi = Array.ofDim[Int](n, n)
    var idx = 0
    for (i <- 0 until n)
      for (j <- i until n) {
        igi(i)(j) = idx
        igi(j)(i) = idx
        idx += 1
      }
    var x = indexedGene(0, 0, 0)
    val tot = (n * (n - 1)) / 2 + n
    override def hasNext: Boolean = x.index < tot

    override def next(): indexedGene = {
      val rv = x
      if (x.second == n - 1) {
        val new_first = x.first + 1
        x = indexedGene(x.index + 1, new_first, new_first)
      } else {
        x = indexedGene(x.index + 1, x.first, x.second + 1)
      }
      rv
    }
  }

  class indexedGenotypeIterator2(n: Int) extends Iterator[indexedGene] {
    var x = indexedGene(0, 0, 0)
    val tot = (n * (n - 1)) / 2 + n
    override def hasNext: Boolean = x.index < tot

    override def next(): indexedGene = {
      val rv = x
      if (x.second > x.first) {
        x = indexedGene(x.index, x.second, x.first)
      } else if (x.first == n - 1) {
        val new_first = x.second + 1
        x = indexedGene(x.index + 1, new_first, new_first)
      } else {
        x = indexedGene(x.index + 1, x.second, x.first + 1)
      }
      rv
    }
  }

  class indexedGenotypeIterator3(n: Int) extends Iterator[indexedGene] {
    var x = (-1, n - 1, -1) // trick start value to have the first iteration give the right first value

    override def hasNext: Boolean = x._2 < n - 1 || x._3 < n - 1

    override def next(): indexedGene = {
      val (i, f, s) = x
      x =
        if (f < s) {
          (i, s, f)
        } else if (f == n - 1) {
          (i + 1, s + 1, s + 1)
        } else {
          (i + 1, s, f + 1)
        }
      new indexedGene(x)
    }
  }

  def genotypeGivenAlleleFreq(freqs: List[Double]): Factor = {
    val numAlleles: Int = freqs.length
    val scopeVals = 1 to (numAlleles * (numAlleles - 1) / 2 + numAlleles ) map (_.toDouble) toList
    val xs: Iterator[Double] = for (ig <- new indexedGenotypeIterator(numAlleles))
      yield (if (ig.first != ig.second) 2 else 1) * freqs(ig.first) * freqs(ig.second)
    Factor(List(Scope("gene", scopeVals)), Values(xs toList))
  }

  def genotypeGivenAlleleFreq2(freqs: List[Double]): Factor = {
    val numAlleles: Int = freqs.length
    val numGenes = numAlleles * (numAlleles - 1) / 2 + numAlleles
    val scopeVals = 1 to numGenes map (_.toDouble) toList
    var xs = Array.ofDim[Double](numGenes)
    for (ig <- new indexedGenotypeIterator2(numAlleles))
      xs(ig.index) += freqs(ig.first) * freqs(ig.second)
    Factor(List(Scope("gene", scopeVals)), Values(xs toList))
  }


  /**
    * Compute phenotype dependence factor on genotype where there are two alleles.  0 maps to having the trait
    * @param dominant indicates whether having the trait requires both alleles to be 1
    * @param geneScope the gene for which we are computing this
    * @return
    */
  def phenoTypeFactor(dominant: Boolean, geneScope: Scope, valName: String): Factor = {
    val xs = if (dominant) {
      List(1, 0, 1, 0, 0, 1) map (_.toDouble)
    } else {
      List(0, 1, 0, 1, 1, 0) map (_.toDouble)
    }
    new Factor(List(Scope(valName, 0.0 :: 1.0 :: Nil), geneScope), Values(xs))
  }

  def main(args: Array[String]): Unit = {
    phenoTypeFactor(true, Scope("gene1", 1 to 3 map (_.toDouble) toList), "pheno1").print()

    val ggaf = genotypeGivenAlleleFreq(List(0.1, 0.2, 0.7))

    ggaf.print()
    genotypeGivenAlleleFreq(List(0.1, 0.9)).print()
    genotypeGivenAlleleFreq2(List(0.1, 0.9)).print()

    println(new indexedGenotypeIterator2(3) toList)
    println(new indexedGenotypeIterator3(3) toList)
  }
}
