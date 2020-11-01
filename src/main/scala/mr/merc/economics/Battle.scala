package mr.merc.economics

import mr.merc.army.{Warrior, WarriorCompetence}
import mr.merc.army.WarriorCompetence.Militia
import mr.merc.battle.BattleModel
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement.TakeProvince
import mr.merc.economics.BattleReport.{Draw, ReportBattleResult, Side1Won, Side2Won}
import mr.merc.map.GameField
import mr.merc.map.hex.{TerrainHex, TerrainHexField}
import mr.merc.map.objects.House
import mr.merc.map.terrain.{Empty, WaterKind}
import mr.merc.map.terrain.FourSeasonsMapObjects.FourSeasonsHouse
import mr.merc.politics.{Province, State}
import mr.merc.unit.Soldier
import mr.merc.util.MercUtils._

abstract sealed class Battle(worldHexField: TerrainHexField, turn: Int) {

  private var militia: (List[Warrior], List[Warrior]) = (Nil, Nil)

  val allWarriors: List[Warrior]

  def participants: Set[State] = sides._1 ++ sides._2

  lazy val battleWeight: Double = allWarriors.map(_.warriorWeight).sum

  def intersect(battle: Battle): Boolean = provinces.intersect(battle.provinces).nonEmpty

  def provinces: List[Province]

  def additionalProvinces: List[Province]

  def placement: Map[Province, List[Warrior]]

  def setMilitia(militia: List[Warrior]): (List[Warrior], List[Warrior])

  def putWarriors(hexField: TerrainHexField): Unit = {
    placement.foreach { case (province, list) =>
      if (provinces.contains(province)) {
        putWarriorsToProvince(list, province, hexField)
      } else if (additionalProvinces.contains(province)) {
        putWarriorsToAdditionalProvince(list, province, hexField)
      } else sys.error(s"province $province doesn't belong to provinces $provinces or additional $additionalProvinces")
    }
  }

  def putMilitiaSoldiers(hexField: TerrainHexField): List[Warrior] = {
    def randomMilitiaSoldier(culture: Culture, owner: State): Warrior = {
      val selectedType = culture.warriorViewNames.possibleWarriors.collect {
        case ((wt, wc), _) if wc == WarriorCompetence.Militia => wt
      }.randomElement()
      new Warrior(selectedType, Militia, culture, owner)
    }

    val seq = for {
      hex <- hexField.hexes
      house <- hex.mapObj.collect { case hs: House => hs }
    } yield {
      hex.province match {
        case Some(p) =>
          if (p.owner == p.controller) {
            val w = randomMilitiaSoldier(house.culture, p.owner)
            hex.soldier = Some(w.soldier)
            Some(w)
          } else None
        case None =>
          sys.error(s"Province must be present in hex $hex in hexField $hexField")
      }
    }

    seq.flatten.toList
  }

  def putWarriorsToProvince(warriors: List[Warrior], province: Province, hexField: TerrainHexField): Unit = {
    val (x, y) = hexField.hexes.filter(_.province.contains(province)).map(h => (h.x, h.y)).medianBySquares
    hexField.closest(hexField.hex(x, y)).filter(h => h.soldier.isEmpty).
      filter(x => x.terrain != Empty && x.terrain.kind != WaterKind).zip(warriors).foreach {
      case (hex, warrior: Warrior) =>
      hex.soldier = Some(warrior.soldier)
    }
  }

  def putWarriorsToAdditionalProvince(warriors: List[Warrior], province: Province, hexField: TerrainHexField): Unit = {
    val intersection = hexField.hexes.filter(_.province.contains(province)).toSet
    require(intersection.nonEmpty, s"province $province doesn't belong to hexField $hexField")

    hexField.closest(intersection).filter(_.soldier.isEmpty).filter(_.terrain != Empty).zip(warriors).foreach { case (hex, warrior) =>
      hex.soldier = Some(warrior.soldier)
    }
  }

  def terrainHexFieldFromProvinces(worldHexField: TerrainHexField): TerrainHexField = {
    val provinceHexes = provinces.flatMap(_.hexes).map(f => worldHexField.hex(f.x, f.y)).toSet
    val neigs = provinceHexes.flatMap(p => worldHexField.neighboursSet(p))
    val additionalHexes = additionalProvinces.flatMap(_.hexes).map(f => worldHexField.hex(f.x, f.y)).toSet & neigs
    val allHexes = provinceHexes ++ additionalHexes

    val minX = allHexes.minBy(_.x).x
    val minY = allHexes.minBy(_.y).y
    val maxX = allHexes.maxBy(_.x).x
    val maxY = allHexes.maxBy(_.y).y

    val allHexesMap = allHexes.map(h => (h.x, h.y) -> h).toMap

    def copyField(x: Int, y: Int): TerrainHex = {
      allHexesMap.get(x + minX, y + minY) match {
        case Some(originalHex) =>
          val hex = new TerrainHex(x, y, originalHex.terrain)
          hex.mapObj = originalHex.mapObj
          hex.province = originalHex.province
          hex.soldier = None
          hex
        case None => new TerrainHex(x, y, Empty)
      }
    }

    new TerrainHexField(maxX - minX + 2, maxY - minY + 2, copyField)
  }

  lazy val gameField: GameField = {
    val battleHexField = terrainHexFieldFromProvinces(worldHexField)
    val mil = putMilitiaSoldiers(battleHexField)
    this.militia = setMilitia(mil)
    putWarriors(battleHexField)
    val (first, second) = sides
    val allSides = Set(first.map(_.toPlayer), second.map(_.toPlayer))
    new GameField(battleHexField, allSides.flatten.toList, allSides)
  }

  def sides: (Set[State], Set[State])

  def isOver: Boolean = new BattleModel(gameField).isOver

  def concludeBattle(actions: WorldStateDiplomacyActions): BattleReport = {
    provinces.foreach { p =>
      val alreadyInProvince = p.regionWarriors.allWarriors.toSet
      val inBattle = this.allWarriors.toSet
      val takeFromProvince = alreadyInProvince & inBattle
      p.regionWarriors.takeWarriors(takeFromProvince.toList)
    }

    val model = new BattleModel(gameField)
    require(model.isOver, "battle must be over")

    val report = this match {
      case one: OneProvinceBattle =>
        val result: ReportBattleResult = if (one.allAttackers.exists(_.isAlive)) {
          val newController = whoToTakeProvince(actions, one.province, one.attackersSet)
          one.province.controller = newController
          Side1Won
        } else if (one.allDefenders.exists(_.isAlive)) {
          Side2Won
        } else Draw

        val (side1Survived, side1Lost) = one.allAttackers.partition(_.isAlive)
        val (side2Survived, side2Lost) = one.allDefenders.partition(_.isAlive)

        val warriors = one.allWarriors.filter(_.isAlive)
        one.province.regionWarriors.receiveWarriors(warriors)

        BattleReport(List(one.province), one.attackersSet.toList, one.defendersSet.toList, result,
          side1Survived, side2Survived, side1Lost, side2Lost, militia._1, militia._2, turn)

      case two: TwoProvinceBattle =>
        val result: ReportBattleResult = if (two.allAttackers1.exists(_.isAlive)) {
          val newController = whoToTakeProvince(actions, two.province1, two.attackers1Set)
          two.province1.controller = newController
          two.province1.regionWarriors.receiveWarriors(two.allAttackers1.filter(_.isAlive))
          Side1Won
        } else if (two.allAttackers2.exists(_.isAlive)) {
          val newController = whoToTakeProvince(actions, two.province2, two.attackers2Set)
          two.province2.controller = newController
          two.province2.regionWarriors.receiveWarriors(two.allAttackers2.filter(_.isAlive))
          Side2Won
        } else Draw

        val (side1Survived, side1Lost) = two.allAttackers1.partition(_.isAlive)
        val (side2Survived, side2Lost) = two.allAttackers2.partition(_.isAlive)

        BattleReport(two.provinces, two.attackers1Set.toList, two.attackers2Set.toList, result,
          side1Survived, side2Survived, side1Lost, side2Lost, militia._1, militia._2, turn)

      case rebellion: RebellionOneProvinceBattle =>
        val currentController = rebellion.province.owner
        val result = if (rebellion.rebels.exists(_.isAlive)) {
          rebellion.province.owner = rebellion.rebelState
          rebellion.province.controller = rebellion.rebelState
          val newWarriors = rebellion.rebels.filter(_.isAlive)
          rebellion.province.regionWarriors.receiveWarriors(newWarriors)
          Side1Won
        } else Side2Won

        val (side1Survived, side1Lost) = rebellion.rebels.partition(_.isAlive)
        val (side2Survived, side2Lost) = rebellion.loyalists.partition(_.isAlive)

        actions.newRebelStateArised(currentController, rebellion.rebelState, rebellion.province)

        BattleReport(List(rebellion.province), List(currentController), List(rebellion.rebelState), result,
          side1Survived, side2Survived, side1Lost, side2Lost, militia._1, militia._2, turn)
    }

    actions.recordBattle(report)

    report
  }

  private def whoToTakeProvince(actions: WorldStateDiplomacyActions, province: Province, winners: Set[State]): State = {
    val allWars = actions.diplomacyEngine.wars.filter(_.containsSides(sides._1, sides._2))
    if (province.controller != province.owner && allWars.exists(_.onSameSide(winners + province.owner))) {
      province.owner
    } else {
      allWars.flatMap(_.targets).collectFirst {
        case tp: TakeProvince if tp.province == province => tp
      } match {
        case Some(tp) => tp.demander
        case None =>
          val armyPower = allWarriors.groupBy(_.owner).map { case (k, v) => k -> v.map(_.warriorWeight).sum }.filterKeys(winners.contains)
          val state = armyPower.maxBy(_._2)._1
          actions.diplomacyEngine.getOverlord(state).getOrElse(state)
      }
    }
  }

}

case class BattleReport(provincesInBattle: List[Province], side1: List[State], side2: List[State], result: ReportBattleResult, side1Survived: List[Warrior],
                        side2Survived: List[Warrior], side1Lost: List[Warrior], side2Lost: List[Warrior],
                        side1Militia: List[Warrior], side2Militia: List[Warrior], turn: Int)

object BattleReport {

  sealed trait ReportBattleResult

  object Side1Won extends ReportBattleResult

  object Side2Won extends ReportBattleResult

  object Draw extends ReportBattleResult

}

class OneProvinceBattle(worldHexField: TerrainHexField, val province: Province, val attackers: Map[Province, List[Warrior]], val defenders: List[Warrior],
                        val additionalDefenders: Map[Province, List[Warrior]], turn: Int) extends Battle(worldHexField, turn) {

  override val allWarriors: List[Warrior] = defenders ++ attackers.values.flatten ++ additionalDefenders.values.flatten

  override def provinces: List[Province] = List(province)

  override def setMilitia(militia: List[Warrior]): (List[Warrior], List[Warrior]) = {
    (Nil, militia)
  }

  def additionalProvinces: List[Province] = (attackers.keySet ++ additionalDefenders.keySet).toList

  override def placement: Map[Province, List[Warrior]] = {
    import cats.implicits._
    Map(province -> defenders) |+| attackers |+| additionalDefenders
  }

  val allAttackers: List[Warrior] = attackers.values.flatten.toList

  val allDefenders: List[Warrior] = defenders ++ additionalDefenders.values.flatten

  val attackersSet: Set[State] = attackers.values.flatten.map(_.owner).toSet

  val defendersSet: Set[State] = {
    val set = defenders.map(_.owner).toSet ++ additionalDefenders.values.flatten.map(_.owner)
    if (set.isEmpty) Set(province.controller)
    else set
  }

  override def sides: (Set[State], Set[State]) = {
    (attackersSet, defendersSet)
  }
}

class TwoProvinceBattle(worldHexField: TerrainHexField, val province1: Province, val province2: Province, val province1Attackers: Map[Province, List[Warrior]],
                        val province1Defenders: List[Warrior], val province1AdditionalDefenders: Map[Province, List[Warrior]],
                        val province2Attackers: Map[Province, List[Warrior]], val province2Defenders: List[Warrior],
                        val province2AdditionalDefenders: Map[Province, List[Warrior]], turn: Int) extends Battle(worldHexField, turn) {

  override val allWarriors: List[Warrior] = province1Defenders ++ province1Attackers.values.flatten ++
    province1AdditionalDefenders.values.flatten ++ province2Attackers.values.flatten ++ province2Defenders ++
    province2AdditionalDefenders.values.flatten

  override def provinces: List[Province] = List(province1, province2)

  override lazy val battleWeight: Double = allWarriors.map(_.warriorWeight).sum +
    province1Attackers.getOrElse(province2, Nil).map(_.warriorWeight).sum +
    province2Attackers.getOrElse(province1, Nil).map(_.warriorWeight).sum

  def additionalProvinces: List[Province] = (province1Attackers.keySet ++ province2Attackers.keySet ++
    province1AdditionalDefenders.keySet ++ province2AdditionalDefenders.keySet).toList

  override def placement: Map[Province, List[Warrior]] = {
    import cats.implicits._
    Map(province1 -> province1Defenders, province2 -> province2Defenders) |+| province1Attackers |+|
      province2Attackers |+| province1AdditionalDefenders |+| province2AdditionalDefenders
  }

  override def setMilitia(militia: List[Warrior]): (List[Warrior], List[Warrior]) = {
    militia.partition(_.owner == province1.owner)
  }

  val attackers1Set: Set[State] = province2Defenders.map(_.owner).toSet ++
    province1Attackers.values.flatten.map(_.owner) ++ province2AdditionalDefenders.values.flatten.map(_.owner)

  val attackers2Set: Set[State] = province1Defenders.map(_.owner).toSet ++
    province2Attackers.values.flatten.map(_.owner) ++ province1AdditionalDefenders.values.flatten.map(_.owner)

  val sides: (Set[State], Set[State]) = {
    require(attackers1Set.nonEmpty, "Attackers 1 set must be non-empty")
    require(attackers2Set.nonEmpty, "Attackers 2 set must be non-empty")
    (attackers1Set, attackers2Set)
  }

  val allAttackers1: List[Warrior] = province2Defenders ++ province1Attackers.values.flatten ++ province2AdditionalDefenders.values.flatten

  val allAttackers2: List[Warrior] = province1Defenders ++ province2Attackers.values.flatten ++ province1AdditionalDefenders.values.flatten

}

class RebellionOneProvinceBattle(worldHexField: TerrainHexField, val province: Province, val rebelState: State,
                                 val loyalists: List[Warrior], turn: Int) extends Battle(worldHexField, turn) {
  private val houses = province.hexes.filter(_.mapObj.exists(_.isInstanceOf[FourSeasonsHouse]))

  private def generateRebel: Warrior = {
    val selectedType = rebelState.primeCulture.warriorViewNames.possibleWarriors.collect {
      case ((wt, wc), _) if wc == WarriorCompetence.Militia => wt
    }.randomElement()
    new Warrior(selectedType, Militia, rebelState.primeCulture, rebelState)
  }

  override def setMilitia(militia: List[Warrior]): (List[Warrior], List[Warrior]) = (Nil, Nil)

  val rebels: List[Warrior] = houses.map(_ => generateRebel).toList

  override val allWarriors: List[Warrior] = rebels ::: loyalists

  override def provinces: List[Province] = List(province)

  override def additionalProvinces: List[Province] = Nil

  override def placement: Map[Province, List[Warrior]] = Map(province -> allWarriors)

  override def putWarriors(hexField: TerrainHexField): Unit = {
    putWarriorsToProvince(loyalists, province, hexField)
  }

  override def putMilitiaSoldiers(hexField: TerrainHexField): List[Warrior] = {
    val hexesWithHouses = hexField.hexes.filter(_.mapObj.exists(_.isInstanceOf[House]))
    hexesWithHouses.zip(rebels).foreach { case (hex, w) =>
      hex.soldier = Some(w.soldier)
    }
    Nil
  }

  override def sides: (Set[State], Set[State]) = {
    val r = rebels.map(_.owner).toSet
    val l = {
      val loyal = loyalists.map(_.owner).toSet
      if (loyal.isEmpty) Set(province.owner)
      else loyal
    }
    (r, l)
  }
}
