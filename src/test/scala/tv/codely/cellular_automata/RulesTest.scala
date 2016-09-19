package tv.codely.cellular_automata

import org.scalatest._
import org.scalatest.Matchers._

final class RulesTest extends WordSpec with GivenWhenThen {

  "Rules" should {
    "be able to evolve an organism applying rule-90" in {

      val initialOrganism = List(true, true, false, true, false, true, false)

      val expectedOrganismEvolution = List(true, true, false, false, false, false, true)

      val actualOrganismEvolution = Evolver.evolveStep(
        initialState = initialOrganism,
        rule = Rules.rule90
      )

      actualOrganismEvolution shouldBe expectedOrganismEvolution
    }
  }

  "Evolver" should {
    "evolve an organism as times as specified" in {

      val initialOrganism = List(true, true, false, true, false, true, false)

      val expectedOrganismEvolution = List(
        List(true, true, false, true, false, true, false),
        List(true, true, false, false, false, false, true),
        List(true, true, true, false, false, true, false),
        List(true, false, true, true, true, false, true),
        List(false, false, true, false, true, false, false),
        List(false, true, false, false, false, true, false)
      )

      val actualOrganismEvolution = Evolver.evolve(
        initialState = initialOrganism,
        rule = Rules.rule90,
        steps = 5
      )

      actualOrganismEvolution shouldBe expectedOrganismEvolution
    }
  }
}
