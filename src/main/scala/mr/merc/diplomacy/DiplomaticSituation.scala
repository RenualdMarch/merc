package mr.merc.diplomacy

import mr.merc.diplomacy.Claim.{ProvinceClaim, StrongProvinceClaim, WeakProvinceClaim}
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement
import mr.merc.diplomacy.DiplomaticSituation.{Hates, Likes, RivalPair}
import mr.merc.economics.WorldConstants
import mr.merc.politics.{Province, State}

class DiplomaticSituation(diplomacy:WorldDiplomacy) {

  def rivalsPairs:Set[RivalPair] = {
    val provinceClaims:List[ProvinceClaim] = diplomacy.allClaims.collect {
      case c:StrongProvinceClaim if c.province.owner != c.state => c
      case c:WeakProvinceClaim if c.province.owner != c.state => c
    }.toList

    provinceClaims.flatMap { pc =>
      val first = pc.province.owner
      val second = pc.state
      Set(RivalPair(first, second), RivalPair(second, first))
    }.toSet
  }

  def hatePairs: Set[Hates] = {
    diplomacy.states.flatMap { state =>
      diplomacy.relationships(state, diplomacy.currentTurn).collect {
        case (otherState, relations) if relations <= WorldConstants.Diplomacy.RelationsForBeingEnemy =>
          Hates(state, otherState)
      }
    }
  }

  def likePairs:Set[Likes] = {
    diplomacy.states.flatMap { state =>
      diplomacy.relationships(state, diplomacy.currentTurn).collect {
        case (otherState, relations) if relations >= WorldConstants.Diplomacy.RelationsForBeingFriend =>
          Likes(state, otherState)
      }
    }
  }

  def shareBorder(state1: State, state2:State):Boolean = {
    diplomacy.regions.exists { p =>
      p.neighbours.exists(n =>
        (p.owner == state1 && n.owner == state2) ||
          (p.owner == state2 && n.owner == state1))
    }
  }

  def rivals(state:State):List[State] = {
    rivalsPairs.collect {
      case RivalPair(`state`, other) => other
    }.toList
  }

  def haters(state: State):List[State] = {
    hatePairs.filter(_.whomIsHated == state).map(_.whoHates).toList
  }

  def likers(state: State): List[State] = {
    likePairs.filter(_.whomIsLiked == state).map(_.whoLikes).toList
  }

  def likersMinusFriends(state: State): List[State] = {
    likers(state) diff friends(state)
  }

  def hatersWithoutRivals(state: State): List[State] = {
    haters(state) diff rivals(state)
  }

  def friends(state: State): List[State] = {
    val pairs = likePairs
    likers(state).filter { liker =>
      pairs.contains(Likes(state, liker))
    }
  }

  def inWar(state:State):List[WarAgreement] = diplomacy.wars(state)

  def neighbours(state: State):List[State] = {
    diplomacy.regions.filter(_.owner == state).flatMap(_.neighbours.map(_.owner)).distinct
  }

  def neighbouringProvince(state:State, p:Province): Boolean = {
    if (p.owner == state) false
    else diplomacy.regions.filter(_.owner == state).flatMap(_.neighbours).contains(p)
  }

  def areRivals(state1:State, state2: State): Boolean = {
    val pairs = rivalsPairs
    pairs.contains(RivalPair(state1, state2)) || pairs.contains(RivalPair(state2, state1))
  }

  def neighbouringProvinces(state: State):List[Province] = {
    val original = provinces(state).toSet
    (original.flatMap(_.neighbours) -- original).toList
  }

  def areRivalsOfRivals(state1:State, state2: State): Boolean = {
    rivalsOfRivals(state1).contains(state2)
  }

  def isInWar(state: State): Boolean = {
    this.inWar(state).nonEmpty
  }

  def powerDifference(from: State, to:State):Double = {
    val toPower = statePower(to)
    val fromPower = statePower(from)
    if (toPower == 0) Double.PositiveInfinity
    else fromPower / toPower
  }

  def statePower(state:State):Double = {
    import mr.merc.economics.WorldConstants.Diplomacy._
    stateGDP(state) * GDPPowerMultiplier + stateArmyTotalLevel(state) * WarriorPowerMultiplier
  }

  private def stateGDP(state:State):Double = {
    diplomacy.regions.filter(_.owner == state).map(_.gdp).sum
  }

  def provinces(state: State):List[Province] = diplomacy.regions.filter(_.owner == state)

  def stateArmyTotalLevel(state: State): Double = {
    diplomacy.regions.flatMap(_.regionWarriors.allWarriors.filter(_.owner == state)).map { w =>
      w.hpPercentage * w.warriorWeight
    }.sum
  }

  def rivalsOfRivals(state: State):List[State] = {
    val rivalsList = rivals(state)
    val rivalsOfRivalsSet = rivalsList.flatMap(rivals).toSet -- rivalsList - state
    rivalsOfRivalsSet.toList
  }

}

object DiplomaticSituation {

  case class RivalPair(rival1: State, rival2: State)

  case class Likes(whoLikes: State, whomIsLiked: State)

  case class Hates(whoHates: State, whomIsHated: State)
}