package tv.codely.cellular_automata

import org.scalatest._
import org.scalatest.Matchers._
import tv.codely.cellular_automata.CellularAutomata.{AliveCell, DeadCell}

final class RulesTest extends WordSpec with GivenWhenThen {

  "Rules" should {
    "be able to evolve an organism applying rule-90" in {

      val initialOrganism = List(AliveCell, AliveCell, DeadCell, AliveCell, DeadCell, AliveCell, DeadCell)

      val expectedOrganismEvolution = List(AliveCell, AliveCell, DeadCell, DeadCell, DeadCell, DeadCell, AliveCell)

      val actualOrganismEvolution = Evolver.evolveStep(
        initialState = initialOrganism,
        rule = Rules.rule90
      )

      actualOrganismEvolution shouldBe expectedOrganismEvolution
    }
  }

  "Evolver" should {
    "evolve an organism as times as specified" in {

      val initialOrganism = List(AliveCell, AliveCell, DeadCell, AliveCell, DeadCell, AliveCell, DeadCell)

      val expectedOrganismEvolution = List(
        List(AliveCell, AliveCell, DeadCell, AliveCell, DeadCell, AliveCell, DeadCell),
        List(AliveCell, AliveCell, DeadCell, DeadCell, DeadCell, DeadCell, AliveCell),
        List(AliveCell, AliveCell, AliveCell, DeadCell, DeadCell, AliveCell, DeadCell),
        List(AliveCell, DeadCell, AliveCell, AliveCell, AliveCell, DeadCell, AliveCell),
        List(DeadCell, DeadCell, AliveCell, DeadCell, AliveCell, DeadCell, DeadCell),
        List(DeadCell, AliveCell, DeadCell, DeadCell, DeadCell, AliveCell, DeadCell)
      )

      val actualOrganismEvolution = Evolver.evolve(
        initialState = initialOrganism,
        rule = Rules.rule90,
        steps = 5
      )

      actualOrganismEvolution shouldBe expectedOrganismEvolution
    }
  }

  "Renderer" should {
    "transform died cells for spaces and alive ones for x" in {

      val organismEvolutions = List(
        List(AliveCell, AliveCell, DeadCell, AliveCell, DeadCell, AliveCell, DeadCell),
        List(AliveCell, AliveCell, DeadCell, DeadCell, DeadCell, DeadCell, AliveCell),
        List(AliveCell, AliveCell, AliveCell, DeadCell, DeadCell, AliveCell, DeadCell)
      )

      val expectedOrganismEvolutionsRendered = "xx x x \nxx    x\nxxx  x "

      val actualOrganismEvolutionsRendered = Renderer.render(organismEvolutions)

      actualOrganismEvolutionsRendered shouldBe expectedOrganismEvolutionsRendered
    }
  }
}
