package net.kasterma.pgm

import scala.util.Random

class Halt extends Exception

case class Transition(val from: String, val to: String, val p: Double)

class FSM(val states: List[String], val transition_ps: List[Transition], val obss: List[Transition]) {
  val rnd = new Random()
  var cur_state = states.head

  def apply(xyp: List[Transition]): String = {
    val possible_xyp = xyp filter (_.from == cur_state)
    if (possible_xyp.isEmpty) {
      throw new Halt()
    }
    val ps: List[Double] = possible_xyp map (_.p)
    val tot_p = ps sum
    val p = rnd.nextDouble() * tot_p
    val idx: Int = (ps.scanLeft(0.0)(_ + _) indexWhere (_ > p)) - 1
    possible_xyp(idx).to
  }

  def step = {
    cur_state = apply(transition_ps)
  }

  def observe: String = {
    apply(obss)
  }
}

object HMM {
  def main(args: Array[String]): Unit = {
    println("hi")
    val fsm = new FSM(List("hi", "ho"), List(Transition("hi", "hi", 0.2), Transition("hi", "ho", 0.2), Transition("ho", "hi", 1.0)),
      List(Transition("hi", "hi-1", 0.5), Transition("hi", "hi-2", 0.5), Transition("ho", "ho", 1.0)))
    println(fsm.cur_state)
    fsm.step
    println(fsm.cur_state)
    fsm.step
    println(fsm.cur_state)
    fsm.step
    println(fsm.cur_state)
    println(1 to 2000 map (_ => {fsm.step; fsm.observe}) groupBy(identity) mapValues(_.length))
  }

}
