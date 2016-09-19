package tv.codely.cellular_automata

import org.scalatest._
import org.scalatest.Matchers._

final class RulesTest extends WordSpec with GivenWhenThen {

  "Rule90" should {
    "evolve the cell properly" in {

      val initialCellsState = List(true, true, false, true, false, true, false)

      val expectedCellsEvolution = List(true, true, false, false, false, false, true)

      val actualCellsEvolution = Rules.evolveStep(
        initialState = initialCellsState,
        rule = Rules.rule90
      )

      actualCellsEvolution shouldBe expectedCellsEvolution
    }
  }
}
