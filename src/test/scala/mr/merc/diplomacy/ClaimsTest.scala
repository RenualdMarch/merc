package mr.merc.diplomacy

import mr.merc.diplomacy.Claim.{StrongProvinceClaim, WeakProvinceClaim}
import mr.merc.economics.WorldConstants

class ClaimsTest extends AbstractDiplomacyTest {


  test("claims generation") {
    val List(first, second, third) = states

    val List(firstProvince1, firstProvince2) = actions.regions.filter(_.owner == first)
    val List(secondProvince1, secondProvince2) = actions.regions.filter(_.owner == second)
    val List(thirdProvince1, thirdProvince2) = actions.regions.filter(_.owner == third)

    val diplomacyEngine = new WorldDiplomacy(actions)

    diplomacyEngine.allClaims shouldBe Set()

    diplomacyEngine.generateInitialStrongClaimsForOwnedTerritories()

    diplomacyEngine.allClaims shouldBe Set(
      StrongProvinceClaim(first, firstProvince1), StrongProvinceClaim(first, firstProvince2),
      StrongProvinceClaim(second, secondProvince1), StrongProvinceClaim(second, secondProvince2),
      StrongProvinceClaim(third, thirdProvince1), StrongProvinceClaim(third, thirdProvince2),
    )

    (0 until 1000).foreach { _ =>
      diplomacyEngine.generateInitialClaimsForNeighbours()
    }

    diplomacyEngine.allClaims shouldBe Set(
      StrongProvinceClaim(first, firstProvince1), StrongProvinceClaim(first, firstProvince2),
      StrongProvinceClaim(second, secondProvince1), StrongProvinceClaim(second, secondProvince2),
      StrongProvinceClaim(third, thirdProvince1), StrongProvinceClaim(third, thirdProvince2),
      WeakProvinceClaim(first, secondProvince1, 100), WeakProvinceClaim(first, secondProvince2, 100),
      WeakProvinceClaim(first, thirdProvince1, 100), WeakProvinceClaim(first, thirdProvince2, 100),
      WeakProvinceClaim(second, firstProvince1, 100), WeakProvinceClaim(second, firstProvince2, 100),
      WeakProvinceClaim(second, thirdProvince1, 100), WeakProvinceClaim(second, thirdProvince2, 100),
      WeakProvinceClaim(third, secondProvince1, 100), WeakProvinceClaim(third, secondProvince2, 100),
      WeakProvinceClaim(third, firstProvince1, 100), WeakProvinceClaim(third, firstProvince2, 100),
    )

    secondProvince1.owner = first
    secondProvince1.controller = first

    diplomacyEngine.replaceWeakClaimsWithStrongClaimsForOwnedTerritories(WorldConstants.Diplomacy.WeakClaimTime - 1)

    diplomacyEngine.allClaims shouldBe Set(
      StrongProvinceClaim(first, firstProvince1), StrongProvinceClaim(first, firstProvince2),
      StrongProvinceClaim(second, secondProvince1), StrongProvinceClaim(second, secondProvince2),
      StrongProvinceClaim(third, thirdProvince1), StrongProvinceClaim(third, thirdProvince2),
      StrongProvinceClaim(first, secondProvince1), WeakProvinceClaim(first, secondProvince2, 100),
      WeakProvinceClaim(first, thirdProvince1, 100), WeakProvinceClaim(first, thirdProvince2, 100),
      WeakProvinceClaim(second, firstProvince1, 100), WeakProvinceClaim(second, firstProvince2, 100),
      WeakProvinceClaim(second, thirdProvince1, 100), WeakProvinceClaim(second, thirdProvince2, 100),
      WeakProvinceClaim(third, secondProvince1, 100), WeakProvinceClaim(third, secondProvince2, 100),
      WeakProvinceClaim(third, firstProvince1, 100), WeakProvinceClaim(third, firstProvince2, 100),
    )
  }
}
