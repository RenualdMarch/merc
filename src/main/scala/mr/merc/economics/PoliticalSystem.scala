package mr.merc.economics

import mr.merc.politics.{Party, Regime, StateElectionReport}

import MapUtil.FloatOperations._

class PoliticalSystem(startingRulingParty: Party) {
  private val ElectionThreshold = 0.03

  private var _rulingParty = startingRulingParty

  private var _parliament: Option[ParliamentParties] = {
    if (startingRulingParty.regime == Regime.Absolute) None
    else Some(ParliamentParties(Map(startingRulingParty -> 1.0d), Set(startingRulingParty)))
  }

  def parliament:Option[ParliamentParties] = _parliament

  def rulingParty: Party = _rulingParty

  def applyElectionResults(election: StateElectionReport): Unit = {
    val resultsAfterThreshold = election.votes.filter(_._2 >= ElectionThreshold).scaleToSum(1d)
    val coalition = findCoalition(resultsAfterThreshold)
    _rulingParty = coalition.maxBy(resultsAfterThreshold)
    _parliament = Some(ParliamentParties(resultsAfterThreshold, coalition))
  }

  def findCoalition(results:Map[Party, Double]): Set[Party] = {
    def variants(alreadyInCoalition: List[Party], remaining: List[Party]):List[List[Party]] = {
      if (alreadyInCoalition.map(results).sum > 0.5) List(alreadyInCoalition)
      else {
        remaining.flatMap { r =>
          variants(r :: alreadyInCoalition, remaining.filterNot(_ == r))
        }
      }
    }

    variants(Nil, results.keySet.toList).minBy(set => (coalitionIdeologyDifference(set), -set.map(results).sum)).toSet
  }

  private def coalitionIdeologyDifference(parties: List[Party]): Int = {
    parties match {
      case List(_) => 0
      case _ =>
        val diffs = for {
        p1 <- parties
        p2 <- parties if p1 != p2
      } yield p1.politicalPosition.diffWithPosition(p2.politicalPosition)

        diffs.max
    }

  }

  def changeAbsoluteRulingParty(newParty: Party): Unit = {
    require(newParty.regime == Regime.Absolute, s"new party must be absolute but is $newParty")
    require(rulingParty.regime == Regime.Absolute, s"not absolute ruling party")
    _rulingParty = newParty
  }

  def usurpPower(newParty: Party): Unit = {
    if (newParty.regime == Regime.Absolute && rulingParty.regime == Regime.Constitutional) {
      _rulingParty = newParty
      _parliament = None
    } else if(newParty.regime == Regime.Constitutional && rulingParty.regime == Regime.Democracy) {
      _rulingParty = newParty
      _parliament = _parliament.map {p =>
        ParliamentParties((p.parties + (newParty -> 1.05)).scaleToSum(1d), Set(newParty))
      }
    } else {
      sys.error(s"party $newParty instad of $rulingParty is incorrect usurpation")
    }
  }

  def giveUpPower(newParty: Party): Unit = {
    if (newParty.regime == Regime.Constitutional && rulingParty.regime == Regime.Absolute) {
      _rulingParty = newParty
      _parliament = Some(ParliamentParties(Map(newParty -> 1.0d), Set(newParty)))
    } else if (newParty.regime == Regime.Democracy && rulingParty.regime == Regime.Constitutional) {
      _rulingParty = newParty
      _parliament = _parliament.map {p =>
        ParliamentParties((p.parties + (newParty -> 1.05)).scaleToSum(1d), Set(newParty))
      }
    } else {
      sys.error(s"party $newParty instad of $rulingParty is incorrect giving up power")
    }
  }


}

case class ParliamentParties(parties:Map[Party, Double], coalition: Set[Party])
