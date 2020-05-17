package mr.merc.economics

import mr.merc.economics.Population.{Craftsmen, Labourers, Lower, Middle, Upper}
import mr.merc.economics.Products._
import mr.merc.log.Logging
import mr.merc.map.generator.WorldMapGenerator
import mr.merc.players.{ColorGenerator, NamesGenerator}
import mr.merc.politics.{Election, Party, PoliticalViews, Province, State}
import mr.merc.util.WeightedRandom
import WorldConstants.Enterprises._
import WorldGenerationConstants._
import mr.merc.army.WarriorCompetence.Professional
import mr.merc.army.{Warrior, WarriorType}
import mr.merc.diplomacy.Claim.{StrongProvinceClaim, WeakProvinceClaim}
import mr.merc.economics.PopulationMigrationOutsideProvince.PopulationMovementBetweenProvinces
import mr.merc.map.terrain.FourSeasonsTerrainTypes._

import scala.util.Random

class WorldGenerator(field:FourSeasonsTerrainHexField) {

  type ConnectivityMap = Map[FourSeasonsTerrainHex, Set[FourSeasonsTerrainHex]]

  implicit class ImplicitConnectivityMap(map:ConnectivityMap) {
    def minusConnectivity(set: Set[FourSeasonsTerrainHex]):ConnectivityMap = {
      map.transform {case (_, s) =>
        s -- set
      } -- set
    }

    def retainConnectivity(set: Set[FourSeasonsTerrainHex]):ConnectivityMap = {
      map.transform {case (_, s) =>
          s & set
      }.filterKeys(set.contains)
    }
  }

  private var colorStream = ColorGenerator.colorStream
  private val namesGenerators:Map[Culture, NamesGenerator] = Culture.cultures.map { c =>
    c -> new NamesGenerator(c.cultureInfo)
  } toMap

  private val resourceGenerator = new WeightedRandom(ResourcesRarity)
  private val factoriesGenerator = new WeightedRandom(FactoriesRarity)

  private def generateEnterprises(province: Province):List[Enterprise] = {
    val resources = generateResources.map {
      case f:FarmProduct => new Farm(f, province, FarmStartingResources, ResourceExtractionEfficiency)
      case m:ResourceProduct => new Mine(m, province, MineStartingResources, ResourceExtractionEfficiency)
    }

    val churches = generateProductsForChurches(province.regionPopulation.cultureMembers.keys.toList).map {
      r => new Church(r, province, ChurchStartingResources, ChurchRitualEfficiency)
    }

    val factories = generateProductsForFactories.map { f =>
      new IndustrialFactory(province, f, FactoryStartingLevel, FactoryStartingMoney,
        FactoryStartingResources, FactoryInputMultiplier, FactoryOutputMultiplier)
    }

    val mages = new MagicGuild(province, FactoryStartingMoney, MagicGuildStartingResources,
      MagicGuildEfficiency)

    resources ++ churches ++ factories ++ List(mages)
  }

  private def generateResources:List[GatheredProduct] = {
    val n = MinResourcesPerRegion + Random.nextInt(MaxResourcesPerRegion - MinResourcesPerRegion + 1)
    resourceGenerator.uniqueRandomItems(n).toList
  }

  private def generateProductsForFactories:List[IndustryProduct] = {
    val n = MinFactoriesPerRegion + Random.nextInt(MaxFactoriesPerRegion - MinFactoriesPerRegion + 1)
    factoriesGenerator.uniqueRandomItems(n).toList
  }

  private def generateProductsForChurches(cultures:List[Culture]):List[Ritual] = {
    cultures.map(Ritual.apply)
  }

  private def generateWarriors(province: Province):List[Warrior] = {
    val culture = province.owner.primeCulture
    val possibleWarriors = culture.warriorViewNames.possibleWarriors.keySet.map(_._1)
    val random = new WeightedRandom[WarriorType](possibleWarriors.map(_ -> 1d).toMap)
    val count = province.regionPopulation.populationCount / WarriorPerPopulation
    random.nextRandomItems(count).map(wt => new Warrior(wt, Professional, culture, province.owner))
  }

  private def generateRegionPops(culture: Culture): RegionPopulation = {

    val pq = MinProvinceSize + Random.nextInt(MaxProvinceSize - MinProvinceSize)

    val pops = Population.populationTypesByClass.flatMap { case (cls, tps) =>
      tps.map { tp =>
        val (moneyPerPerson, countMultiplier, literacy) = cls match {
          case Lower if tp == Craftsmen => (PoorMoneyPerPerson, MiddleMultiplier, PoorLiteracy)
          case Lower if tp == Labourers => (PoorMoneyPerPerson, 2 * MiddleMultiplier, PoorLiteracy)
          case Lower => (PoorMoneyPerPerson, PoorMultiplier, PoorLiteracy)
          case Middle => (MiddleMoneyPerPerson, MiddleMultiplier, MiddleLiteracy)
          case Upper => (RichMoneyPerPerson, RichMultiplier, RichLiteracy)
        }

        val count = countMultiplier * pq / 2 + Random.nextInt(countMultiplier * pq / 2)
        val views = PoliticalViews.initPoliticalViews(tp)
        new Population(culture, tp, count, count * moneyPerPerson, (count * literacy).toInt, views)
      }
    }

    new RegionPopulation(pops.toList)
  }

  private def generateRegionMarket:RegionMarket = {
    val prices = Products.AllProducts.map {
      case p:GatheredProduct => p -> 1d
      case p:IndustryProduct => p -> 2d
    }.toMap
    new RegionMarket(prices)
  }

  private def generateState(culture: Culture):State = {
    val color = colorStream.head
    colorStream = colorStream.tail
    val name = namesGenerators(culture).stateNames.extract()
    new State(name, culture, StateStartingMoney, new PoliticalSystem(Party.aristocratic), color)
  }

  private def divideIntoContinuousParts(connectivityMap: ConnectivityMap, sizes:List[Int]):List[ConnectivityMap] = {
    val (_, resultList) = sizes.foldLeft((connectivityMap, List[ConnectivityMap]())) {case ((map, list), size) =>
        val part = extractContinuousPart(map, size)
        val remainingCM = map minusConnectivity part
        val currentCM = map retainConnectivity part

      (remainingCM, currentCM :: list)
    }
    resultList.reverse
  }

  private def generateCultureStateDivisions(connectivityMap: Map[FourSeasonsTerrainHex, Set[FourSeasonsTerrainHex]]):List[(State, List[FourSeasonsTerrainHex])] = {
    import mr.merc.util.Divide.DivideIntegral
    /*val provincesPerCulture = Culture.cultures zip (connectivityMap.size divList Culture.cultures.size)
    val raceSizes = provincesPerCulture.groupBy(_._1.race).toList.map {case (race, list) =>
      race -> list.map(_._2).sum
    }*/

    val races = Culture.cultures.map(_.race).distinct
    val raceSizes = races zip (connectivityMap.size divList races.size)

    val racesZipCM = raceSizes.map(_._1) zip divideIntoContinuousParts(connectivityMap, raceSizes.map(_._2))
    racesZipCM.flatMap {case (race, raceMap) =>
      val cultures = Random.shuffle(Culture.cultures.filter(_.race == race))
      val cultureSizes = raceMap.size divList cultures.size
      val cultureZipMaps = cultures zip divideIntoContinuousParts(raceMap, cultureSizes)
      cultureZipMaps.flatMap { case (culture, cultureMap) =>
        val provincesPerCulture = cultureMap.size
        val avgStatesSize = if (provincesPerCulture < StateAvgProvinces) 1
          else provincesPerCulture / StateAvgProvinces
        val states = provincesPerCulture divList avgStatesSize
        import mr.merc.util.MercUtils._
        val shuffled = states.shuffle(StateShuffles).toList
        val statesMapParts = divideIntoContinuousParts(cultureMap, shuffled)

        statesMapParts.map { hexes =>
          generateState(culture) -> hexes.keys.toList
        }
      }
    }
  }

  def generateStateAndProvinces(provincesHexes: Map[FourSeasonsTerrainHex, Set[FourSeasonsTerrainHex]]): Map[State, List[Province]] = {
    val connectivityMap = WorldGenerator.buildConnectivityMap(field, provincesHexes)
    val divisions = generateCultureStateDivisions(connectivityMap)

    val result = divisions.map{case (state, capitals) =>
      state -> capitals.map {capital =>
        val name = namesGenerators(state.primeCulture).cityNames.extract()
        val p = new Province(name, state, generateRegionMarket, generateRegionPops(state.primeCulture), provincesHexes(capital), capital)
        p.enterprises = generateEnterprises(p).toVector
        p.regionWarriors.receiveWarriors(generateWarriors(p))
        provincesHexes(capital).foreach(_.province = Some(p))
        p
      }
    } toMap
    val provinces = result.values.flatten.toList
    provinces.foreach { p =>
      val neigs = connectivityMap(p.capital).map(c => provinces.find(_.capital == c).get)
      p.initNeighbours(neigs)
    }
    (0 until PopMigrationsToNeighbours).foreach {_ =>
      mixPopulations(provinces)
    }

    result.foreach { case (state, provinces) =>
      val party = Election.mostPopularParty(provinces.flatMap(_.regionPopulation.pops), Party.allParties)
      state.politicalSystem.rulingParty = party
    }

    result
  }

  private def mixPopulations(provinces:List[Province]): Unit = {
    provinces.flatMap { province =>
      province.regionPopulation.pops.flatMap { pop =>
        val neigbours = province.neighbours
        neigbours.map{ neig =>
          val count = PopMigrationToNeighbourPercentage * pop.populationCount / neigbours.size
          val targetPop = neig.regionPopulation.pop(pop.populationType, pop.culture)
          PopulationMovementBetweenProvinces(pop, province, targetPop, neig, count.toInt)
        }
      }
    }.foreach(_.applyMovement())

  }

  private def extractContinuousPart(connectivityMap:ConnectivityMap, extractionSize:Int):Set[FourSeasonsTerrainHex] = {
    require(isContinuousPiece(connectivityMap), "Connectivity map must be continuous piece")
    require(connectivityMap.nonEmpty, "connectivity map must be non-empty")
    require(connectivityMap.size >= extractionSize, s"Connectivity map size is ${connectivityMap.size} but extraction is $extractionSize")

    if (connectivityMap.size == extractionSize) {
      return connectivityMap.keySet
    }

    class Step(parent: Option[Step], selectedHex: FourSeasonsTerrainHex) {

      lazy val depth: Int = parent match {
        case None => 1
        case Some(p) => 1 + p.depth
      }

      lazy val allHexes: Set[FourSeasonsTerrainHex] = parent match {
        case None => Set(selectedHex)
        case Some(s) => s.allHexes + selectedHex
      }

      lazy val allNeighbours:Set[FourSeasonsTerrainHex] = {
        allHexes.flatMap(connectivityMap) -- allHexes
      }

      lazy val isValid: Boolean = {
        depth <= extractionSize &&
          isContinuousPiece(connectivityMap minusConnectivity allHexes) &&
          isContinuousPiece(connectivityMap retainConnectivity allHexes)
      }

      lazy val connectivity:Int = {
        val sizes1 = (connectivityMap minusConnectivity allHexes).values.map(_.size)
        val sizes2 = (connectivityMap retainConnectivity allHexes).values.map(_.size)
        sizes1.sum + sizes2.sum
      }

      lazy val isSolution:Boolean = {
        depth == extractionSize && isValid
      }

      lazy val children: Stream[Step] = {
        allNeighbours.map { hex =>
          new Step(Some(this), hex)
        }.toList.sortBy(-_.connectivity).toStream
      }

      def findSolution:Option[Step] = {
        if (isSolution)
          Some(this)
        else if (!isValid)
          None
        else {
          children.flatMap(_.findSolution).headOption
        }
      }
    }

    val answer = connectivityMap.keySet.toList.map { h =>
      new Step(None, h)
    }.sortBy(-_.connectivity).toStream.flatMap(_.findSolution)

    answer.headOption match {
      case Some(h) => h.allHexes
      case None => throw new RuntimeException(s"Failed to find answer for connectivity map $connectivityMap with size ${connectivityMap.size} with extraction size $extractionSize")
    }
  }

  // everyone with everyone
  private def isContinuousPiece(connectivityMap:ConnectivityMap): Boolean = {
    var currentSet:Set[FourSeasonsTerrainHex] = connectivityMap.headOption match {
      case None => Set()
      case Some((capital, _)) => Set(capital)
    }

    var prevSet = Set[FourSeasonsTerrainHex]()
    while (currentSet != prevSet) {
      prevSet = currentSet
      currentSet = currentSet.flatMap(connectivityMap) ++ currentSet
    }

    currentSet == connectivityMap.keySet
  }
}

object WorldGenerator extends Logging {

  def generateWorld(): WorldState = {
    val timeBefore = System.currentTimeMillis()
    val world = WorldMapGenerator.generateWorldMap(WorldMapWidth, WorldMapHeight, Provinces)
    val generator = new WorldGenerator(world.terrain)
    val r = (generator.generateStateAndProvinces(world.provinces), world.terrain)
    val playerState = r._1.keys.head
    val ws = new WorldState(r._1.values.flatten.toList, playerState, world.terrain, generator.namesGenerators, generator.colorStream)
    // TODO battles are not played!!! possible source of errors
    0 until TradeDaysBeforeStart foreach(_ => ws.nextTurn())
    info(s"World generation took ${(System.currentTimeMillis() - timeBefore) / 1000d} seconds")
    ws.diplomacyEngine.generateInitialStrongClaimsForOwnedTerritories()
    ws.diplomacyEngine.generateInitialClaimsForNeighbours()
    ws.initialAiDiplomacy()
    ws
  }

  def buildConnectivityMap(field:FourSeasonsTerrainHexField, map:Map[FourSeasonsTerrainHex, Set[FourSeasonsTerrainHex]]):Map[FourSeasonsTerrainHex, Set[FourSeasonsTerrainHex]] = {
    map.map { case (capital, provinceHexes) =>
      val allNeigs = provinceHexes.filterNot(_.terrainMap == FourSeasonsWater).flatMap(h => field.neighbours(h)) -- provinceHexes
      capital -> allNeigs.flatMap {n =>
        map.find(_._2.contains(n)).map(_._1)
      }
    }
  }

}