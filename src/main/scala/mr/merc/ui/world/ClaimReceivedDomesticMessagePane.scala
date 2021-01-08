package mr.merc.ui.world

import mr.merc.diplomacy.Claim
import mr.merc.diplomacy.Claim.{StrongProvinceClaim, VassalizationClaim, WeakProvinceClaim}
import mr.merc.economics.Seasons
import mr.merc.local.Localization
import mr.merc.politics.State
import org.tbee.javafx.scene.layout.MigPane
import scalafx.scene.text.{Font, FontWeight, Text}
import scalafx.Includes._

class ClaimReceivedDomesticMessagePane(receiver: State, claim: Claim) extends MigPane {

  claim match {
    case WeakProvinceClaim(state, province, until) =>
      if (state == receiver) {
        add(BigText(Localization("messages.claims.weHaveWeakClaim")))
      } else {
        add(new StateComponentColorName(state))
        add(BigText(Localization("messages.claims.someoneHasWeakClaim")))
      }
      add(new Text {
        text = province.name
        font = Font(Font.default.family, FontWeight.Bold, Components.largeFontSize)
      }.delegate)

      add(new StateComponentColorName(province.owner))
      add(BigText(Localization("messages.claims.until")))
      add(BigText(Seasons.date(until).localizedString))

    case StrongProvinceClaim(state, province) =>
      if (state == receiver) {
        add(BigText(Localization("messages.claims.weHaveStrongClaim")))
      } else {
        add(new StateComponentColorName(state))
        add(BigText(Localization("messages.claims.someoneHasStrongClaim")))
      }

      add(new Text {
        text = province.name
        font = Font(Font.default.family, FontWeight.Bold, Components.largeFontSize)
      }.delegate)

      add(new StateComponentColorName(province.owner))

    case VassalizationClaim(state, possibleVassal, claimTurnEnd) =>
      add(new StateComponentColorName(state))
      add(BigText(Localization("messages.claims.vassalizationClaim")))
      add(new StateComponentColorName(possibleVassal))
      add(BigText(Localization("messages.claims.until")))
      add(BigText(Seasons.date(claimTurnEnd).localizedString))
  }
}
