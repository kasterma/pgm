package net.kasterma.pgm

case class Plate(idx: List[String])

object PlateModel{
  def apply(plates: List[Plate], graph: Graph, plate_assignment: Map[String, List[String]]): PlateModel =
    new PlateModel(plates, graph, plate_assignment)
}

class PlateModel(val plates: List[Plate], val graph: Graph, val plate_assignment: Map[String, List[String]]) {
  def full_eval(plate_instances: Map[String, List[String]]): ValueGraph = {
    ValueGraph(Map.empty, graph.edges)
  }
}

object ValueGraph {
  def apply(vals: Map[String, Int], edges: List[Edge]): ValueGraph = new ValueGraph(vals, edges)
  def empty = new ValueGraph(Map.empty, List.empty)
}

class ValueGraph(val vals: Map[String, Int], val edges: List[Edge]) {

}

object Plates {
  def main(args: Array[String]): Unit = {
    println("Starting plates")

    val plate_model_1 = PlateModel(List(Plate(List("course")), Plate(List("student"))),
      Graph(Map("difficulty" -> new ConstantNode(3),
        "intelligence" -> new ConstantNode(4),
        "grade" -> new BinaryNode((x,y) => x + y)),
        List(Edge("difficulty", "grade", 0), Edge("intelligence", "grade", 1))),
      Map("course" -> List("difficulty", "grade"), "student" -> List("intelligence", "grade")))

    val vg = plate_model_1.full_eval(Map("course" -> List("econ101", "cs101"), "student" -> List("jan", "jaap", "jimbo")))

    println(vg)
  }
}
