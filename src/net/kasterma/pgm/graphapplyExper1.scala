package net.kasterma.pgm

import java.util.NoSuchElementException

/**
  * Created by kasterma on 15/09/16.
  */

object graphapplyExper1 {
  abstract class Node {
    def get_value: Option[Int]
    def set_input(x: Option[Int], idx: Int): Boolean
  }

  class ConstantNode(val value: Int) extends Node {
    def get_value = Some(value)
    def set_input(x: Option[Int], idx: Int) = false
  }

  class UnaryNode(val f: Int => Int) extends Node {
    var input: Array[Option[Int]] = Array(None)
    def set_input(x: Option[Int], idx: Int): Boolean = if (idx == 0) {input(idx) = x; true} else false
    def get_value = input(0).map(f)
  }

  class BinaryNode(val f: (Int, Int) => Int) extends Node {
    var inputs: Array[Option[Int]] = Array(None, None)
    def set_input(x: Option[Int], idx: Int) = if (idx == 0 | idx == 1) {inputs(idx) = x; true} else false
    def get_value = try {
      Some(f(inputs(0).get, inputs(1).get))
    } catch {
      case _: NoSuchElementException => None
    }
  }

  class Rule(val r: Map[Int, Int], val value: Int) {
    def test(test_map: Map[Int, Option[Int]]): Boolean =
      r.forall {case (x,y) => test_map.contains(x) & y == test_map(x).get}
  }

  class RuleNode(val rules: List[Rule]) extends Node {
    var inputs: Map[Int, Option[Int]] = Map[Int, Option[Int]]()
    def set_input(x: Option[Int], idx: Int) = {
      inputs += idx -> x
      true
    }
    def get_value = {
      val applicable_rules = rules filter ((rule:Rule) => rule.test(inputs))
      if (applicable_rules.length != 1) {
        None
      } else {
        Some(applicable_rules.head.value)
      }
    }
  }

  case class Edge(from_node: String, to_node: String, to_idx: Int)

  class Graph (val nodes: Map[String, Node], val edges: List[Edge]) {
    def apply(at_node: String): Option[Int] = {
      val nodeo = nodes.get(at_node)
      if (nodeo.isDefined) {
        val node = nodeo.get
        val parents = edges filter (_.to_node == at_node)
        parents map (e => node.set_input(apply(e.from_node), e.to_idx))
        node.get_value
      } else {
        None
      }
    }
  }

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
}
