package net.kasterma.pgm

/**
  * Created by kasterma on 12/10/16.
  */
object geneticExamples {

  case class indexedGene(index: Int, first: Int, second: Int)

  class indexedGenotypeIterator(n: Int) extends Iterator[indexedGene] {
    var current = indexedGene(0, 0, 0)

    override def hasNext: Boolean = current.index < n * n

    override def next(): indexedGene = {
      val rv = current
      if (current.first == n - 1) {
        current = indexedGene(current.index + 1, 0, current.second + 1)
      } else {
        current = indexedGene(current.index + 1, current.first + 1, current.second)
      }
      rv
    }
  }

  def genotypeGivenAlleleFreq(freqs: List[Double]): Factor = {
    val numAlleles: Int = freqs.length
    val scopeVals = 1 to numAlleles map (_.toDouble) toList
    val xs: Iterator[Double] = for (ig <- new indexedGenotypeIterator(numAlleles)) yield freqs(ig.first) * freqs(ig.second)
    Factor(List(Scope("gene1", scopeVals), Scope("gene2", scopeVals)), Values(xs toList))
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
  }
}
