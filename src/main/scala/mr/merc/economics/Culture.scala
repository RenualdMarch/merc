package mr.merc.economics

import mr.merc.army.WarriorViewNames
import mr.merc.economics.Population._
import mr.merc.economics.Products._
import mr.merc.economics.WorldConstants.Population.needsQ
import mr.merc.map.objects.{House, HumanCityHouse, HumanCottage, HumanVillageHouse}
import scalafx.scene.paint.Color
import MapUtil.FloatOperations._

object Culture {

  def cultures: List[Culture] = List(LatinHuman, KnightHuman, DarkHuman, GreekHuman, GermanHuman) //, HighElf, DarkElf, BarbarianOrc, RockDwarf, GreenSaurian, OldDrakes, Forsaken, RedDemons)

  // slavic - first to add

  //
  // DISABLED CULTURES
  //
  /*case object HighElf extends Culture("state.federation", Elves)
  case object DarkElf extends Culture("state.confederation", Elves)
  case object BarbarianOrc extends Culture("state.horde", Orcs)
  case object RockDwarf extends Culture("state.clan", Dwarfs)
  case object GreenSaurian extends Culture("state.syndicate", Saurians)
  case object OldDrakes extends Culture("state.dominion", Drakes)
  case object Forsaken extends Culture("state.collective", Undead)
  case object RedDemons extends Culture("state.legion", Demons)
  */
  // good words: alliance, protectorate, tribe, army, despotia

  case class StateForm(monarchy: String, democracy: String)

  case class CultureInfo(stateForm: StateForm, cities: List[String], states: List[String])

  case object LatinHuman extends Culture("latin", Humans, HumanCityHouse, Color.Red) {
    val warriorViewNames: WarriorViewNames = WarriorViewNames.LatinCulture

    val cultureInfo: CultureInfo = CultureInfo(StateForm("empire", "republic"),
      List("Aarhusium",
        "Aemona",
        "Alexandria",
        "Algeris",
        "Antunnacum",
        "Aquae Sextiae",
        "Arae Flaviae",
        "Arbela",
        "Ascalon",
        "Athenae",
        "Augusta",
        "Treverorum",
        "Vindelicorum",
        "Berytus",
        "Bostonia",
        "Botrus",
        "Byblos",
        "Camulodunum",
        "Carnium",
        "Carolopolis",
        "Carolinapolis",
        "Cantabrigia",
        "Celeia",
        "Constantia",
        "Cultellus",
        "Flavus",
        "Eboracum",
        "Edessa",
        "Equus Albus",
        "Florentia",
        "Flumen",
        "Gevalia",
        "Hafnia",
        "Halifacium",
        "Heliopolis",
        "Regius",
        "Hustonia",
        "Iuliacum",
        "Leptis Magna",
        "Lutetia",
        "Massalia",
        "Medaba",
        "Mediolanum",
        "Mons Regius",
        "Nova Aurelia",
        "Helvetia",
        "Bedfordia",
        "Urbs Novum",
        "Portus",
        "Olympia",
        "Oxonia",
        "Pechinum",
        "Praetoria",
        "Providenia",
        "Urbs Reginae",
        "Rubricobacilensis",
        "Regiopolis",
        "Sarnia",
        "Sciamhaevum",
        "Sicagum",
        "Sidon",
        "Tonitralis",
        "Tempe",
        "Tiberias",
        "Tingis",
        "Turicum",
        "Tyrus",
        "Vigornia"),
      List("Byzantine",
        "Roman",
        "Gallic",
        "Iberian",
        "Latin",
        "Lux"))
  }

  case object KnightHuman extends Culture("french", Humans, HumanVillageHouse, Color.Blue) {
    val warriorViewNames: WarriorViewNames = WarriorViewNames.WesnothCulture

    val cultureInfo: CultureInfo = CultureInfo(StateForm("kingdom", "republic"),
      List("Argenroux",
        "Camiers",
        "Dornzis",
        "Fortals",
        "Roaris",
        "Avitou",
        "Tullastones",
        "Arnegein",
        "Langenneuve",
        "Romainsee",
        "Boucon",
        "Champiles",
        "Steunten",
        "Aarbeke",
        "Roulon",
        "Oryonne",
        "Ballycrana",
        "Giannifka",
        "Petrouka",
        "Marmi",
        "Apriranto",
        "Siebasso",
        "Acevecchia",
        "Estrecedo",
        "Abralos",
        "Portafanha",
        "Badanaria",
        "Marbelbao",
        "Motava",
        "Achalargos",
        "Kokies",
        "Nafpapoli",
        "Rotera",
        "Maracelio",
        "Trevitonto",
        "Fuga",
        "Ribeibugal",
        "Esmogal",
        "Burrasa",
        "Guadalajon",
        "Bindia",
        "Kalyfka",
        "Melimis",
        "Ilioumia",
        "Bibasso",
        "Tesaro",
        "Acigliano",
        "Ermencamento",
        "Mouco",
        "Guarica",
        "Frono",
        "Laguna",
        "Vadra"),
      List("Brittan",
        "Normandian",
        "Lorraine",
        "Kuteron",
        "Aquitain",
        "Burgundian"))
  }

  case object DarkHuman extends Culture("nilf", Humans, HumanCottage, Color.Black) {
    val warriorViewNames: WarriorViewNames = WarriorViewNames.DarkHumanCulture

    val cultureInfo: CultureInfo = CultureInfo(StateForm("empire", "republic"),
      List("Saalbirge",
        "Kufdorf",
        "Sherpstal",
        "Denderloon",
        "Épilles",
        "Caveil",
        "Immenholz",
        "Lahnnau",
        "Northgheda",
        "Midleway",
        "Teroord",
        "Hilverhem",
        "Bellintal",
        "Bremstein",
        "Ebenschlag",
        "Innskirch",
        "Sherpenburg",
        "Wevelrijk",
        "Bellun",
        "Besanves",
        "Goldbeuren",
        "Mühldorf",
        "Malgheda",
        "Newrary",
        "Doetinwarden",
        "Zaltdrecht",
        "Rheizell",
        "Walensellen",
        "Vilein",
        "Poysschlag",
        "Torwerp",
        "Zoutlare",
        "Charmiers",
        "Narnin",
        "Drolshausen",
        "Alsthal",
        "Banlick",
        "Ballinport",
        "Bredaoord",
        "Genrend",
        "Zollirus",
        "Freientern"),
      List("Vuspeobarian",
        "Escarian",
        "Yathean",
        "Yeprenian",
        "Spuyrus",
        "Graosalian",
        "Echaitan",
        "Ublanian"))
  }

  case object GreekHuman extends Culture("greek", Humans, HumanCityHouse, Color.Gray) {
    val warriorViewNames: WarriorViewNames = WarriorViewNames.GreekDardoCulture

    val cultureInfo: CultureInfo = CultureInfo(StateForm("kingdom", "republic"),
      List("Pergipolis",
        "Pithendos",
        "Gazegea",
        "Sideiros",
        "Myonylos",
        "Reia",
        "Massasa",
        "Katocaea",
        "Byllopolis",
        "Pergopeion",
        "Bouthritake",
        "Boricus",
        "Aegantina",
        "Ereturii",
        "Hateia",
        "Decyrian",
        "Nicoparae",
        "Bourontos",
        "Doralamis",
        "Didacia",
        "Ptelos",
        "Aspenai",
        "Barissa",
        "Abaclea",
        "Androsia",
        "Rhithoy",
        "Akriteia",
        "Tylissea",
        "Crotorus",
        "Bouthroria",
        "Byzamahos",
        "Colastro",
        "Selens",
        "Aphylos",
        "Pyrgera",
        "Massomnos",
        "Tyrios",
        "Assetus",
        "Thermenia",
        "Lapithagra"),
      List("Spartan",
        "Macedon",
        "Greek",
        "Beotian",
        "Trojan",
        "Achaean",
        "Corinthian"))
  }

  case object GermanHuman extends Culture("german", Humans, HumanVillageHouse, Color.Yellow) {
    val warriorViewNames: WarriorViewNames = WarriorViewNames.ChevalierCulture

    val cultureInfo: CultureInfo = CultureInfo(StateForm("empire", "republic"),
      List("Marchbach",
        "Klagensill",
        "Roesden",
        "Brugsen",
        "Fürstensten",
        "Vellfeld",
        "Doneton",
        "Shandee",
        "Vlissingvoort",
        "Hengelem",
        "Neunstatten",
        "Huttlach",
        "Kirchweil",
        "Nieuwten",
        "Haguenne",
        "Freudenkirchen",
        "Dettelburen",
        "Lislee",
        "Shanmore",
        "Vlissingkerk",
        "Slolo",
        "Morson",
        "Waldenringen",
        "Gerasnitz",
        "Vulkerfeld",
        "Vilmuide",
        "Dendersen",
        "Drabagne",
        "Obernkastel",
        "Grobenthal",
        "Castleton",
        "Slomuiden",
        "Marchfelden",
        "Stocksill",
        "Freudenlein",
        "Elterfeld",
        "Dungarbet",
        "Wickvan",
        "Doetinbommel",
        "Hoofdzee",
        "Wadenspach",
        "Gorhofen",
        "Hallzis",
        "Amkirchen",
        "Zotberg",
        "Herberge",
        "Dietenrode",
        "Heilholder",
        "Baldon",
        "Wagegraaf",
        "Windmere",
        "Grandzona",
        "Spreitensingen"),
      List("Saxonian",
        "Alpian",
        "Franconian",
        "Saar",
        "Rhine",
        "Voralbergian",
        "German",
        "Prussian"))
  }

  case object SlavicHuman extends Culture("slavic", Humans, HumanCottage, Color.Violet) {
    val warriorViewNames: WarriorViewNames = null

    val cultureInfo: CultureInfo = CultureInfo(StateForm("tsardom", "republic"),
      List("Asichow",
        "Stolya",
        "Ederenye",
        "Vathei",
        "Lunbaesca",
        "Cernaciu",
        "Snipany",
        "Zhydachach",
        "Almadilna",
        "Vawkabryn",
        "Navabrush",
        "Szazhacika",
        "Lipcadeni",
        "Cantenet",
        "Jastrbrzych",
        "Sosnobin",
        "Sanchet",
        "Videcoi",
        "Chorttrov",
        "Ostrozhany",
        "Orylsk",
        "Birombov",
        "Gukory",
        "Zhelepul",
        "Magnitonaksk",
        "Ivavets",
        "Chebongrad",
        "Zelenapa",
        "Nevisinsk",
        "Noyasensk",
        "Vorotroitsk",
        "Nabedorozhny",
        "Obniya",
        "Nabenezh",
        "Almestal",
        "Borileuz",
        "Zelenovda",
        "Arzaski",
        "Kamytrov"),
      List("Ukrainian",
        "Russian",
        "Polish",
        "Slovak",
        "Czech",
        "Moravian",
        "Bulgarian"))
  }

  private def scaleNeeds(needsToScale: PopulationNeeds): PopulationNeeds = {
    needsToScale.transform { case (popClass, classNeedsMap) =>
      classNeedsMap.transform { case (popNeedsType, needsMap) =>
        val q = needsQ(popClass)(popNeedsType)
        needsMap.scaleToSum(q)
      }
    }
  }

  private def defaultHumanNeeds(culture: Culture): PopulationNeeds = Map(
    Lower -> Map(
      LifeNeeds -> Map(Grain -> 3, Fruit -> 1, Cattle -> 1),
      RegularNeeds -> Map(Tea -> 1, Clothes -> 1, Liquor -> 1, Furniture -> 1, Coal -> 1,
        Lumber -> 1, Ritual(culture) -> 1),
      LuxuryNeeds -> Map(Magic -> 1, Paper -> 1, Coffee -> 1, Weapons -> 1, Wine -> 1, Medicine -> 1)
    ),
    Middle -> Map(
      LifeNeeds -> Map(Grain -> 2, Fruit -> 1, Cattle -> 1),
      RegularNeeds -> Map(Tea -> 1, Clothes -> 1, Liquor -> 1, Furniture -> 1, Coal -> 1, Ritual(culture) -> 1,
        Glass -> 1, Wine -> 1, Magic -> 1, Medicine -> 1, Paper -> 1, Weapons -> 1, Cement -> 1),
      LuxuryNeeds -> Map(Wine -> 1, Magic -> 1, Medicine -> 1,
        Furniture -> 1, Opium -> 1, Paper -> 1, Jewelry -> 1)
    ),
    Upper -> Map(
      LifeNeeds -> Map(Grain -> 1, Fruit -> 1, Cattle -> 2),
      RegularNeeds -> Map(Tea -> 5, Clothes -> 5, Liquor -> 5, Furniture -> 5, Coal -> 10, Cement -> 5,
        Glass -> 5, Wine -> 10, Magic -> 10, Medicine -> 5, Paper -> 10, Jewelry -> 5, Weapons -> 5, Opium -> 5, Ritual(culture) -> 1),
      LuxuryNeeds -> Map(Furniture -> 5, Coal -> 5, Paper -> 5, Magic -> 5,
        Medicine -> 5, Weapons -> 5, Cement -> 5, Opium -> 5, Ritual(culture) -> 2, Jewelry -> 5)
    )
  )
}

abstract class Culture(val name: String, val race: Race, val houseStyle: House, val color: Color) {

  import Culture._

  def needs: PopulationNeeds = scaleNeeds(defaultHumanNeeds(this))

  def cultureNameKey: String = "culture." + name

  val warriorViewNames: WarriorViewNames

  val cultureInfo: CultureInfo
}
