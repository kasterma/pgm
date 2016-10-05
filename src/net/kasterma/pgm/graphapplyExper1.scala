package net.kasterma.pgm

import java.util.NoSuchElementException

/**
  * Created by kasterma on 15/09/16.
  */

object graphapplyExper1 {
  def main(args: Array[String]): Unit = {
    val g1 = new Graph(Map("three" -> new ConstantNode(3),
      "four" -> new ConstantNode(4),
      "sum" -> new BinaryNode((x,y) => x + y),
      "prod" -> new BinaryNode((x,y) => x * y),
      "rule" -> new RuleNode(List(new Rule(Map(3 -> 49), 666)))),
      List(Edge("three", "sum", 0), Edge("four", "sum", 1), Edge("sum", "prod", 0), Edge("sum", "prod", 1),
        Edge("prod", "rule", 3)))

    println(g1("three"))
    println(g1("sum"))
    println(g1("prod"))
    println(g1("rule"))

    val cpdt1 = new CPDTree(Left(Split(1, 3),
      new CPDTree(Right(HistogramDistribution[Int](List(1,2), List(0.5, 0.5)))),
      new CPDTree(Right(HistogramDistribution[Int](List(1,2), List(0.1, 0.9))))))
    val t1 = new TreeNode(cpdt1)

    val g2 = new Graph(Map("two" -> new ConstantNode(2),
      "four" -> new ConstantNode(4),
      "tree" -> t1),
      List(Edge("two", "tree", 1)))

    1 to 100 map (_ => g2("tree")) groupBy identity mapValues(_.length)

    val g3 = new Graph(Map("two" -> new ConstantNode(2),
      "four" -> new ConstantNode(4),
      "tree" -> t1),
      List(Edge("four", "tree", 1)))

    1 to 100 map (_ => g3("tree")) groupBy identity mapValues(_.length)
  }
}
