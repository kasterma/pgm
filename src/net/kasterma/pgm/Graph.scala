package net.kasterma.pgm

import scala.language.postfixOps
import scala.annotation.tailrec

/**
  * Created by kasterma on 02/09/16.
  */
class Graph(val nodes: List[String], val edges: List[(String, String)]) {
  type Node = String

  def addNode(label:Node): Graph = {
    new Graph(nodes :+ label, edges)
  }

  def addEdge(e:(Node, Node)): Graph = {
    val new_nodes = (nodes :+ e._1 :+ e._2).distinct
    val new_edges = edges :+ e
    new Graph(new_nodes, new_edges)
  }

  override def toString: Node = {
    "(nodes: " + nodes.mkString(" ") + ", edges: " + (edges.map {case (a,b) => a + "->" + b } mkString " ") + ")"
  }

  def get_out_edges(ns: Set[Node]): Set[(Node, Node)] = {
    edges filter {case (s,t) => ((ns contains s) & !(ns contains t)) | (!(ns contains s) & (ns contains t))} toSet
  }

  def get_out_edges(ns: List[Node]): List[(Node, Node)] = {
    edges filter {case (s,t) => ((ns contains s) & !(ns contains t)) | (!(ns contains s) & (ns contains t))}
  }

  def bd(ns:Set[Node]): Set[Node] = {
    val out_edges: List[(Node, Node)] = edges filter {case (s,t) => ns contains s}
    val one_step: List[Node] = out_edges map {case (s, t) => t }
    one_step.toSet.diff(ns)
  }

  @tailrec final def mst_h(t: Graph, out_edges: List[(Node, Node)]): Graph = {
    if (out_edges.isEmpty) {
      t
    } else {
      val nt: Graph = t addEdge out_edges.head
      mst_h(nt, get_out_edges(nt.nodes))
    }
  }

  def mst:Graph = {
    if (nodes.isEmpty) {
      Graph.empty
    } else {
      mst_h(Graph(nodes.head), get_out_edges(List(nodes.head)))
    }
  }
}

object Graph {
  type Node = String
  def empty: Graph = new Graph(List.empty, List.empty)

  def apply(node: Node, nodes: Node*): Graph = fromNodes(List(node) ++ nodes)

  def apply(edge: (Node, Node), edges: (Node, Node)*): Graph = fromEdges(List(edge) ++ edges)

  def apply(nodes: List[Node], edges: List[(Node, Node)]): Graph = {
    val nodes_in_edges = edges.flatMap(_.productIterator).toSet
    if (!nodes_in_edges.subsetOf(nodes.toSet)) {
      println("Danger will robinson: you have edges between nodes that do not exist")
    }
    new Graph(nodes, edges)
  }

  def fromNodes(nodes: List[Node]): Graph = new Graph(nodes, List.empty)

  def fromEdges(edges: List[(Node, Node)]): Graph =
    new Graph(edges.flatMap {case (x,y) => List(x,y)}.distinct, edges)
}

// import net.kasterma.pgm._
// val student_network = Graph("D" -> "G", "I" -> "G", "I" -> "S", "G" -> "L")
// student_network.mst