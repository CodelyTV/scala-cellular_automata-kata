package tv.codely.cellular_automata

import tv.codely.cellular_automata.CellularAutomata._

object CellularAutomata {
  type Evolutions = List[Organism]

  abstract class Cell
  case object AliveCell extends Cell
  case object DeadCell extends Cell

  case class CellsChunk(first: Cell, second: Cell, third: Cell)

  case class Organism(rawOrganism: List[Cell]) {
    def wrappedWithDeadCells: Organism = Organism(List(DeadCell) ++ rawOrganism ++ List(DeadCell))

    def chunks: List[CellsChunk] = rawOrganism.sliding(3).toList.map {
      case List(left, center, right) => CellsChunk(left, center, right)
    }
  }
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
    Organism(initialState.wrappedWithDeadCells.chunks.map(rule))
  }

  def evolve(initialState: Organism, rule: CellsChunk => Cell, steps: Int): Evolutions = {
    (1 to steps).toList.scanLeft(initialState)((state, _) => evolveStep(state, rule))
  }
}

object Renderer {
  val translations: Map[Cell, String] = Map(DeadCell -> " ", AliveCell -> "x")

  def render(organismEvolutions: Evolutions): String = {
    organismEvolutions.map(organism => organism.rawOrganism.map(translations).mkString).mkString("\n")
  }
}
