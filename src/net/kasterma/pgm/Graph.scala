package net.kasterma.pgm

import java.util.NoSuchElementException

import scala.language.postfixOps
import scala.annotation.tailrec


case class Edge(from_node: String, to_node: String, to_idx: Int)

object Graph {
  def apply(nodes: Map[String, Node], edges: List[Edge]): Graph = new Graph(nodes, edges)
}

class Graph (val nodes: Map[String, Node], val edges: List[Edge]) {
  def apply(at_node: String): Option[Int] = apply(at_node, Map.empty)

  def apply(at_node: String, values: Map[String, Int]): Option[Int] = {
    try {
      Some(values(at_node))
    } catch {
      case e: NoSuchElementException =>
        val nodeo = nodes.get (at_node)
        if (nodeo.isDefined) {
          val node = nodeo.get
          val parents = edges filter (_.to_node == at_node)
          parents map (e => node.set_input (apply (e.from_node, values), e.to_idx) )
          node.get_value
        } else {
          None
        }
    }
  }
}
