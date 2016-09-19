package tv.codely.cellular_automata

import tv.codely.cellular_automata.CellularAutomata.{Evolutions, Organism}

case class Chunk(firstValue: Boolean, secondValue: Boolean, thirdValue: Boolean)

object CellularAutomata {
  type Organism = List[Boolean]
  type Evolutions = List[Organism]
}

object Rules {
  def rule90(chunk: Chunk): Boolean = chunk match {
    case Chunk(false, false, false) => false
    case Chunk(false, false, true) => true
    case Chunk(false, true, false) => false
    case Chunk(false, true, true) => true
    case Chunk(true, false, false) => true
    case Chunk(true, false, true) => false
    case Chunk(true, true, false) => true
    case Chunk(true, true, true) => false
  }
}

object Evolver {
  def evolveStep(initialState: Organism, rule: Chunk => Boolean): Organism = {
    val wrappedInitialState = List(false) ++ initialState ++ List(false)

    val chunkList = wrappedInitialState.sliding(3).map { rawChunk =>
      val List(firstValue, secondValue, thirdValue) = rawChunk

      Chunk(firstValue, secondValue, thirdValue)
    }

    chunkList.map(rule).toList
  }

  def evolve(initialState: Organism, rule: Chunk => Boolean, steps: Int): Evolutions = {
    (1 to steps).toList.scanLeft(initialState)((state, _) => evolveStep(state, rule))
  }
}

object Renderer {
  val translations = Map(false -> " ", true -> "x")

  def render(organismEvolutions: Evolutions): String = {
    organismEvolutions.map(organism => organism.map(translations).mkString).mkString("\n")
  }
}
