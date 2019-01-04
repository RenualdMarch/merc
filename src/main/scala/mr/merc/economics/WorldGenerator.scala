package mr.merc.economics

import com.typesafe.config.ConfigFactory
import mr.merc.economics.Population.{Culture, LatinHuman, Lower, Middle, Upper}
import mr.merc.economics.Products._
import mr.merc.map.generator.WorldMapGenerator
import mr.merc.map.hex.{TerrainHex, TerrainHexField}
import mr.merc.map.terrain.WaterKind
import mr.merc.players.{ColorGenerator, NamesGenerator}
import mr.merc.politics.{PoliticalViews, Province, State}
import mr.merc.util.WeightedRandom

import scala.util.Random

class WorldGenerator(field:TerrainHexField) {

  type ConnectivityMap = Map[TerrainHex, Set[TerrainHex]]

  implicit class ImplicitConnectivityMap(map:ConnectivityMap) {
    def minusConnectivity(set: Set[TerrainHex]):ConnectivityMap = {
      map.transform {case (_, s) =>
        s -- set
      } -- set
    }

    def retainConnectivity(set: Set[TerrainHex]):ConnectivityMap = {
      map.transform {case (_, s) =>
          s & set
      }.filterKeys(set.contains)
    }
  }


  import scala.collection.JavaConverters._
  private val config = ConfigFactory.load("conf/worldGenerationEconomics")
  private val economicConfig = ConfigFactory.load("conf/economics.conf")
  private val colorGenerator = new ColorGenerator()
  private val namesGenerators:Map[Culture, NamesGenerator] = Population.cultures.map { c =>
    c -> new NamesGenerator(Culture.cultureConfig(c))
  } toMap

  private val minResources = config.getInt("world.resources.resourcesPerRegion.min")
  private val maxResources = config.getInt("world.resources.resourcesPerRegion.max")
  private val minFactories = config.getInt("world.factories.factoriesPerRegion.min")
  private val maxFactories = config.getInt("world.factories.factoriesPerRegion.max")
  private val startingFactoryResource = config.getInt("world.factories.startingResources.industrial")
  private val startingMagicResource = config.getInt("world.factories.startingResources.magic")
  private val startingFactoryMoney = config.getInt("world.factories.startingMoney")
  private val startingFactoryLevel = config.getInt("world.factories.startingLevel")

  private val farmStartingResource = config.getInt("world.resources.startingResources.farm")
  private val mineStartingResource = config.getInt("world.resources.startingResources.mine")
  private val churchStartingResource = config.getInt("world.resources.startingResources.church")
  private val factoryStartingResource = config.getInt("world.factories.startingResources.industrial")
  private val magicStartingResource = config.getInt("world.factories.startingResources.magic")

  private val resourceGenerator = new WeightedRandom(resourceRarity)
  private val factoriesGenerator = new WeightedRandom(factoriesRarity)

  private def generateEnterprises(province: Province):List[Enterprise] = {
    val resourceEff = economicConfig.getInt("enterprise.resourceEfficiency")
    val resources = generateResources.map {
      case f:FarmProduct => new Farm(f, province, farmStartingResource, resourceEff)
      case m:ResourceProduct => new Mine(m, province, mineStartingResource, resourceEff)
    }

    val churches = generateProductsForChurches(province.regionPopulation.cultureMembers.keys.toList).map {
      r => new Church(r, province, churchStartingResource, economicConfig.getInt("enterprise.churchEfficiency")  )
    }

    val factoryInputEff = economicConfig.getInt("enterprise.factoryInputEfficiency")
    val factoryOutputEff = economicConfig.getInt("enterprise.factoryOutputEfficiency")
    val factories = generateProductsForFactories.map { f =>
      new IndustrialFactory(province, f, startingFactoryLevel, startingFactoryMoney,
        factoryStartingResource, factoryInputEff, factoryOutputEff)
    }

    val guildInputEff = economicConfig.getInt("enterprise.magicInputEfficiency")
    val guildOutputEff = economicConfig.getInt("enterprise.magicOutputEfficiency")

    val mages = generateProductsForMageGuilds.map { m =>
      new MagicGuildEnterprise(province, m, startingFactoryMoney, magicStartingResource, guildInputEff, guildOutputEff)
    }

    resources ++ churches ++ factories ++ mages
  }

  private def resourceRarity: Map[GatheredProduct, Double] = {
    config.getConfig("world.resources.rarity").entrySet().asScala.map { entry =>
      Products.productByName(entry.getKey) -> entry.getValue.unwrapped().asInstanceOf[Number].doubleValue()
    }.collect{case (p: GatheredProduct, d) => p -> d}.toMap
  }

  private def generateResources:List[GatheredProduct] = {
    val n = minResources + Random.nextInt(maxResources - minResources + 1)
    List.fill(n)(resourceGenerator.nextRandomItem())
  }

  private def factoriesRarity: Map[IndustryProduct, Double] = {
    config.getConfig("world.factories.rarity").entrySet().asScala.map { entry =>
      Products.productByName(entry.getKey) -> entry.getValue.unwrapped().asInstanceOf[Number].doubleValue()
    }.collect{case (p: IndustryProduct, d) => p -> d}.toMap
  }

  private def generateProductsForFactories:List[IndustryProduct] = {
    val n = minFactories + Random.nextInt(maxFactories - minFactories + 1)
    List.fill(n)(factoriesGenerator.nextRandomItem())
  }

  private def generateProductsForMageGuilds:List[MagicProduct] = {
    if (Random.nextBoolean()) List(Amulet) else List(Medicine)
  }

  private def generateProductsForChurches(cultures:List[Culture]):List[Ritual] = {
    cultures.map(Ritual.apply)
  }

  private def generateRegionPops(culture: Culture): RegionPopulation = {
    val conf = config.getConfig("world.population")

    val minProvinceSize = conf.getInt("minProvinceSize")
    val maxProvinceSize = conf.getInt("maxProvinceSize")
    val pq = minProvinceSize + Random.nextInt(maxProvinceSize - minProvinceSize)

    val pops = Population.populationTypesByClass.flatMap { case (cls, tps) =>
      tps.map { tp =>
        val (moneyPerPerson, countMultiplier, literacy) = cls match {
          case Lower => (conf.getDouble("poorMoneyPerPerson"), conf.getInt("poorMultiplier"), conf.getDouble("poorLiteracy"))
          case Middle => (conf.getDouble("middleMoneyPerPerson"), conf.getInt("middleMultiplier"), conf.getDouble("middleLiteracy"))
          case Upper => (conf.getDouble("richMoneyPerPerson"), conf.getInt("richMultiplier"), conf.getDouble("richLiteracy"))
        }

        val count = countMultiplier * pq / 2 + Random.nextInt(countMultiplier * pq / 2)
        val views = PoliticalViews.initPoliticalViews(tp, literacy)
        new Population(culture, tp, count, count * moneyPerPerson, (count * literacy).toInt, views)
      }
    }

    new RegionPopulation(pops.toList)
  }

  private def generateRegionMarket:RegionMarket = {
    val prices = Products.AllProducts.map {
      case p:GatheredProduct => p -> 1d
      case p:IndustryProduct => p -> 2d
      case p:MagicProduct => p -> 2d
    }.toMap
    new RegionMarket(prices)
  }

  private def generateState(culture: Culture):State = {
    val color = colorGenerator.nextColor()
    val name = namesGenerators(culture).stateNames.extract()
    State(name, culture, new StateBudget(config.getDouble("world.state.startingMoney")), TaxPolicy.zeroTaxes, color)
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

  private def generateCultureStateDivisions(connectivityMap: Map[TerrainHex, Set[TerrainHex]]):List[(State, List[TerrainHex])] = {
    import mr.merc.util.Divide.DivideIntegral
    val provincesPerCulture = Population.cultures zip (connectivityMap.size divList Population.cultures.size)
    val raceSizes = provincesPerCulture.groupBy(_._1.race).toList.map {case (race, list) =>
      race -> list.map(_._2).sum
    }

    val racesZipCM = raceSizes.map(_._1) zip divideIntoContinuousParts(connectivityMap, raceSizes.map(_._2))
    racesZipCM.flatMap {case (race, raceMap) =>
      val cultures = Population.cultures.filter(_.race == race)
      val cultureSizes = raceMap.size divList cultures.size
      val cultureZipMaps = cultures zip divideIntoContinuousParts(raceMap, cultureSizes)
      cultureZipMaps.flatMap { case (culture, cultureMap) =>
        val provincesPerCulture = cultureMap.size
        val avgProvinces = config.getInt("world.state.avgProvinces")
        val avgStatesSize = if (provincesPerCulture < avgProvinces) 1
          else provincesPerCulture / avgProvinces
        val states = provincesPerCulture divList avgStatesSize
        import mr.merc.util.MercUtils._
        val shuffled = states.shuffle(config.getInt("world.state.shuffles")).toList
        val statesMapParts = divideIntoContinuousParts(cultureMap, shuffled)

        statesMapParts.map { hexes =>
          generateState(culture) -> hexes.keys.toList
        }
      }
    }
  }

  def generateStateAndProvinces(provincesHexes: Map[TerrainHex, Set[TerrainHex]]): Map[State, List[Province]] = {
    val divisions = generateCultureStateDivisions(WorldGenerator.buildConnectivityMap(field, provincesHexes))

    divisions.map{case (state, capitals) =>
      state -> capitals.map {capital =>
        val name = namesGenerators(state.primeCulture).cityNames.extract()
        val p = Province(name, state, generateRegionMarket,generateRegionPops(state.primeCulture), provincesHexes(capital), capital)
        p.enterprises = generateEnterprises(p).toVector
        provincesHexes(capital).foreach(_.province = Some(p))
        p
      }
    } toMap
  }

  private def extractContinuousPart(connectivityMap:ConnectivityMap, extractionSize:Int):Set[TerrainHex] = {
    require(isContinuousPiece(connectivityMap), "Connectivity map must be continuous piece")
    require(connectivityMap.nonEmpty, "connectivity map must be non-empty")
    require(connectivityMap.size >= extractionSize, s"Connectivity map size is ${connectivityMap.size} but extraction is $extractionSize")

    if (connectivityMap.size == extractionSize) {
      return connectivityMap.keySet
    }

    class Step(parent: Option[Step], selectedHex: TerrainHex) {

      lazy val depth: Int = parent match {
        case None => 1
        case Some(p) => 1 + p.depth
      }

      lazy val allHexes: Set[TerrainHex] = parent match {
        case None => Set(selectedHex)
        case Some(s) => s.allHexes + selectedHex
      }

      lazy val allNeighbours:Set[TerrainHex] = {
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
    var currentSet:Set[TerrainHex] = connectivityMap.headOption match {
      case None => Set()
      case Some((capital, _)) => Set(capital)
    }

    var prevSet = Set[TerrainHex]()
    while (currentSet != prevSet) {
      prevSet = currentSet
      currentSet = currentSet.flatMap(connectivityMap) ++ currentSet
    }

    currentSet == connectivityMap.keySet
  }
}

object WorldGenerator {
  private val worldMapWidth = 50
  private val worldMapHeight = 50
  private val hexesPerProvince = 150
  private val provinces = (worldMapHeight * worldMapWidth * WorldMapGenerator.landPercentage / hexesPerProvince).toInt

  private val config = ConfigFactory.load("conf/worldGenerationEconomics")

  def generateWorld(): WorldState = {
    val world = WorldMapGenerator.generateWorldMap(worldMapWidth, worldMapHeight, provinces)
    val generator = new WorldGenerator(world.terrain)
    val r = (generator.generateStateAndProvinces(world.provinces), world.terrain)
    val playerState = r._1.keys.find(_.primeCulture == LatinHuman).get
    val ws = new WorldState(r._1.values.flatten.toList, playerState, world.terrain)
    val iterations = config.getInt("world.tradeDays")
    0 until iterations foreach(_ => ws.nextTurn())
    ws
  }

  def buildConnectivityMap(field:TerrainHexField, map:Map[TerrainHex, Set[TerrainHex]]):Map[TerrainHex, Set[TerrainHex]] = {
    map.map { case (capital, provinceHexes) =>
      val allNeigs = provinceHexes.filterNot(_.terrain.is(WaterKind)).flatMap(h => field.neighbours(h)) -- provinceHexes
      capital -> allNeigs.flatMap {n =>
        map.find(_._2.contains(n)).map(_._1)
      }
    }
  }

}