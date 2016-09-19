package tv.codely.cellular_automata

import tv.codely.cellular_automata.CellularAutomata._

object CellularAutomata {
  type Organism = List[Cell]
  type Evolutions = List[Organism]

  abstract class Cell
  case object AliveCell extends Cell
  case object DeadCell extends Cell

  case class CellsChunk(first: Cell, second: Cell, third: Cell)
}

object Rules {
  def rule90(chunk: CellsChunk): Cell = chunk match {
    case CellsChunk(DeadCell, DeadCell, DeadCell) => DeadCell
    case CellsChunk(DeadCell, DeadCell, AliveCell) => AliveCell
    case CellsChunk(DeadCell, AliveCell, DeadCell) => DeadCell
    case CellsChunk(DeadCell, AliveCell, AliveCell) => AliveCell
    case CellsChunk(AliveCell, DeadCell, DeadCell) => AliveCell
    case CellsChunk(AliveCell, DeadCell, AliveCell) => DeadCell
    case CellsChunk(AliveCell, AliveCell, DeadCell) => AliveCell
    case CellsChunk(AliveCell, AliveCell, AliveCell) => DeadCell
  }
}

object Evolver {
  def evolveStep(initialState: Organism, rule: CellsChunk => Cell): Organism = {
    val wrappedInitialState = List(DeadCell) ++ initialState ++ List(DeadCell)

    val chunkList = wrappedInitialState.sliding(3).map { rawChunk =>
      val List(firstValue, secondValue, thirdValue) = rawChunk

      CellsChunk(firstValue, secondValue, thirdValue)
    }

    chunkList.map(rule).toList
  }

  def evolve(initialState: Organism, rule: CellsChunk => Cell, steps: Int): Evolutions = {
    (1 to steps).toList.scanLeft(initialState)((state, _) => evolveStep(state, rule))
  }
}

object Renderer {
  val translations: Map[Cell, String] = Map(DeadCell -> " ", AliveCell -> "x")

  def render(organismEvolutions: Evolutions): String = {
    organismEvolutions.map(organism => organism.map(translations).mkString).mkString("\n")
  }
}
