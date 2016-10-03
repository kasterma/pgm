package net.kasterma.pgm

/**
  * Dynamic Bayesian Network
  */

class BayesNet {
  def apply(state: String): String = {
    ""
  }
}

class twoTimesliceBayesNet {
  def queries(state: String): List[String] = {
    List.empty
  }

  def eval(state: String, states: Map[String, String]): String = {
    ""
  }
}

class DBN(val init: BayesNet, val step: twoTimesliceBayesNet) {

  /**
    * Simulate a realisation of the state at step t
    * @param state a state in the 2-BN of this DBN
    * @param t an integer indicating number of steps taken
    * @return a string representing the value of the state
    */
  def apply(state: String, t: Int): String = {
    if (t == 0) {
      init(state)
    } else {
      val step_qs: List[String] = step.queries(state)
      val step_as: List[(String, String)] = step_qs map (x => (x, apply(x, t - 1)))
      step.eval(state, step_as.toMap)
    }
  }


}
