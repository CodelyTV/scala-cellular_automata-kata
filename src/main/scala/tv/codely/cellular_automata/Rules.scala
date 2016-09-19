package tv.codely.cellular_automata

case class Chunk(firstValue: Boolean, secondValue: Boolean, thirdValue: Boolean)

object Rules {
  def evolveStep(initialState: List[Boolean], rule: Chunk => Boolean): List[Boolean] = {
    val wrappedInitialState = List(false) ++ initialState ++ List(false)

    val chunkList = wrappedInitialState.sliding(3).map { rawChunk =>
      val List(firstValue, secondValue, thirdValue) = rawChunk

      Chunk(firstValue, secondValue, thirdValue)
    }

    chunkList.map(rule).toList
  }

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
