package tv.codely.cellular_automata

import org.scalatest._
import org.scalatest.Matchers._
import tv.codely.cellular_automata.CellularAutomata.{AliveCell, DeadCell, Organism}

final class RulesTest extends WordSpec with GivenWhenThen {

  "Rules" should {
    "be able to evolve an organism applying rule-90" in {

      val initial = Organism(List(AliveCell, AliveCell, DeadCell, AliveCell, DeadCell, AliveCell, DeadCell))

      val expectedEvolution = Organism(List(AliveCell, AliveCell, DeadCell, DeadCell, DeadCell, DeadCell, AliveCell))

      val actualEvolution = Evolver.evolveStep(initial, Rules.rule90)

      actualEvolution shouldBe expectedEvolution
    }
  }

  "Evolver" should {
    "evolve an organism as times as specified" in {

      val initial = Organism(List(AliveCell, AliveCell, DeadCell, AliveCell, DeadCell, AliveCell, DeadCell))

      val expectedEvolutions = List(
        Organism(List(AliveCell, AliveCell, DeadCell, AliveCell, DeadCell, AliveCell, DeadCell)),
        Organism(List(AliveCell, AliveCell, DeadCell, DeadCell, DeadCell, DeadCell, AliveCell)),
        Organism(List(AliveCell, AliveCell, AliveCell, DeadCell, DeadCell, AliveCell, DeadCell)),
        Organism(List(AliveCell, DeadCell, AliveCell, AliveCell, AliveCell, DeadCell, AliveCell)),
        Organism(List(DeadCell, DeadCell, AliveCell, DeadCell, AliveCell, DeadCell, DeadCell)),
        Organism(List(DeadCell, AliveCell, DeadCell, DeadCell, DeadCell, AliveCell, DeadCell))
      )

      val actualOrganismEvolution = Evolver.evolve(initial, Rules.rule90, steps = 5)

      actualOrganismEvolution shouldBe expectedEvolutions
    }
  }

  "Renderer" should {
    "transform died cells for spaces and alive ones for x" in {

      val organismEvolutions = List(
        Organism(List(AliveCell, AliveCell, DeadCell, AliveCell, DeadCell, AliveCell, DeadCell)),
        Organism(List(AliveCell, AliveCell, DeadCell, DeadCell, DeadCell, DeadCell, AliveCell)),
        Organism(List(AliveCell, AliveCell, AliveCell, DeadCell, DeadCell, AliveCell, DeadCell))
      )

      val expectedOrganismEvolutionsRendered = "xx x x \nxx    x\nxxx  x "

      val actualOrganismEvolutionsRendered = Renderer.render(organismEvolutions)

      actualOrganismEvolutionsRendered shouldBe expectedOrganismEvolutionsRendered
    }
  }
}
