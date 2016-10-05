package net.kasterma.pgm

import java.util.NoSuchElementException

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

case class Split (idx: Int, boundary: Int)

class CPDTree (tree: Either[(Split, CPDTree, CPDTree), HistogramDistribution[Int]]){
  def get_value(inputs: Map[Int, Option[Int]]): Option[Int] = {
    if (tree.isLeft) {
      val value: Option[Int] = inputs(tree.left.get._1.idx)
      if (value.isDefined & value.get < tree.left.get._1.boundary) {
        tree.left.get._2.get_value(inputs)
      } else {
        tree.left.get._3.get_value(inputs)
      }
    } else {
      Some(tree.right.get.sample)
    }
  }
}

class TreeNode(val tree: CPDTree) extends Node {
  var inputs: Map[Int, Option[Int]] = Map[Int, Option[Int]]()
  def set_input(x: Option[Int], idx: Int) = {
    inputs += idx -> x
    true
  }
  def get_value = tree.get_value(inputs)
}