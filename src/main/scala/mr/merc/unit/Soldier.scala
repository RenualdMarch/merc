package mr.merc.unit

import mr.merc.players.Player
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.objects.House
import mr.merc.map.objects.WoodenBridge
import mr.merc.unit.SoldierTypeAttribute._
import mr.merc.map.terrain.TerrainKind._
import SoldierState._

class Soldier(val name: String, initialSoldierType: SoldierType, val owner: Player) {
  private var _soldierType = initialSoldierType
  private var _hp = soldierType.hp
  private var _state = Set[SoldierState]()

  var turnState: SoldierTurnState = NotHisTurn


  def soldierType: SoldierType = _soldierType
  def soldierType_=(value: SoldierType): Unit = {
    val hpIncrease = if (value.hp > soldierType.hp) value.hp - soldierType.hp else 0
    val newHp = if (hp + hpIncrease > value.hp) value.hp else hp + hpIncrease
    _soldierType = value
    hp = newHp
  }

  def hp = _hp
  def hp_=(newHp: Int) {
    _hp = newHp
    if (_hp > soldierType.hp) {
      _hp = soldierType.hp
    } else if (_hp < 0) {
      _hp = 0
    }
  }

  var exp = 0
  var movePointsRemain = soldierType.movement
  var attackedThisTurn = false
  def movedThisTurn = movePointsRemain != soldierType.movement
  def needsHealing = hp != soldierType.hp

  def addState(state: SoldierState) {
    _state += state
  }

  def removeState(state: SoldierState) {
    _state -= state
  }

  def state = _state

  def beforeTurnRenowation() {
    movePointsRemain = soldierType.movement
    attackedThisTurn = false

    if (state.contains(Slowed)) {
      removeState(Slowed)
    }
  }

  // TODO split into methods
  def beforeTurnActions(field: TerrainHexField, x: Int, y: Int): List[BeforeTurnAction] = {
    val attributes = soldierType.attributes
    val result = collection.mutable.ArrayBuffer[BeforeTurnAction]()

    val neighbours = field.neighbours(x, y).flatMap(_.soldier).filter(_.owner == this.owner)
    if (attributes.contains(Cures)) {
      val allies = neighbours.filter(_.state.contains(Poisoned))
      result ++= allies.map(t => CureSoldier(this, t))
    }

    val heal = if (attributes.contains(Heals4)) Some(Heals4)
    else if (attributes.contains(Heals8)) Some(Heals8)
    else None

    heal match {
      case Some(healing) =>
        val whoNeedsHealing = neighbours.filter(_.needsHealing)
        val actions = healing match {
          case Heals4 => whoNeedsHealing.map(Heal4Soldier(this, _))
          case Heals8 => whoNeedsHealing.map(Heal8Soldier(this, _))
          case xx => sys.error(s"Impossible case with $xx")
        }
        result ++= actions
      case None => // do nothing
    }

    if (attributes.contains(Regenerates) &&
      (needsHealing || state.contains(Poisoned))) {
      result += Regeneration(this)
    }

    if (field.hex(x, y).mapObj.exists(_.isInstanceOf[House]) && !attributes.contains(Regenerates)
      && (needsHealing || state.contains(Poisoned))) {
      result += Regeneration(this)
    }

    if (state.contains(Poisoned) && !attributes.contains(Regenerates)
      && !field.hex(x, y).mapObj.exists(_.isInstanceOf[House])) {
      result += PoisoningDamage(this)
    }

    result toList
  }

  def movementCostFunction(hex: TerrainHex): Int = {
    if (hex.mapObj.exists(_.isInstanceOf[House]) || hex.terrain.is(WallsKind)) {
      soldierType.moveCost(WallsKind)
    } else if (hex.mapObj.contains(WoodenBridge) || hex.terrain.is(RoadKind)) {
      soldierType.moveCost(GrassKind)
    } else {
      soldierType.moveCost(hex.terrain.kind)
    }
  }
}

sealed trait SoldierTurnState
case object NotHisTurn extends SoldierTurnState
case object HaventMoved extends SoldierTurnState
case object StillCanMove extends SoldierTurnState
case object CanntMoveAnyMore extends SoldierTurnState
case object HaveAttacked extends SoldierTurnState
