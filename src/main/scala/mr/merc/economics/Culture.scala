package mr.merc.economics

import mr.merc.army.WarriorViewNames
import mr.merc.economics.Population._
import mr.merc.economics.Products._
import mr.merc.economics.WorldConstants.Population.needsQ
import scalafx.scene.paint.Color
import MapUtil.FloatOperations._

object Culture {

  private val humanCultures = List(LatinHuman, FrenchHuman /*, DarkHuman, GreekHuman, GermanHuman, ArabHuman, SlavicHuman, SpainHuman*/)

  private val elvishCultures = List(WoodElves, DesertElves)

  private val orcishCultures = List(WesnothOrcs)

  private val dwarfishCultures = List(WesnothDwarfes)

  private val undeadCultures = List(WesnothUndead)

  // + goblins
  // + trolls
  // + saurians


  val cultures: List[Culture] = humanCultures ++ elvishCultures ++ orcishCultures ++ dwarfishCultures ++ undeadCultures

  //HighElf, DarkElf, BarbarianOrc, RockDwarf, GreenSaurian, OldDrakes, Forsaken, RedDemons)

  //
  // DISABLED CULTURES
  //
  /*case object HighElf extends Culture("state.federation", Elves)
  case object DarkElf extends Culture("state.confederation", Elves)
  case object GreenSaurian extends Culture("state.syndicate", Saurians)
  case object OldDrakes extends Culture("state.dominion", Drakes)
  case object Forsaken extends Culture("state.collective", Undead)
  case object RedDemons extends Culture("state.legion", Demons)
  */
  // good words: alliance, protectorate, tribe, army, despotia

  // desertCity awful in winter - need to put it in desert

  case class StateForm(monarchy: String, democracy: String)

  case class CultureInfo(stateForm: StateForm, cities: List[String], states: List[String])

  case object LatinHuman extends Culture("latin", Humans, "humanCity", Color.Red) {
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

  case object FrenchHuman extends Culture("french", Humans, "humanVillage", Color.Blue) {
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

  case object DarkHuman extends Culture("nilf", Humans, "humanCottage", Color.Black) {
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

  case object GreekHuman extends Culture("greek", Humans, "humanCity", Color.Gray) {
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

  case object GermanHuman extends Culture("german", Humans, "humanVillage", Color.Yellow) {
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

  case object SlavicHuman extends Culture("slavic", Humans, "humanHut", Color.Violet) {
    val warriorViewNames: WarriorViewNames = WarriorViewNames.WolfCulture

    val cultureInfo: CultureInfo = CultureInfo(StateForm("principality", "republic"),
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
        "Belarusian",
        "Galician",
        "Volhynian",
        "Podolian"))
  }

  case object ArabHuman extends Culture("arab", Humans, "humanCottage", Color.Green) {
    override val warriorViewNames: WarriorViewNames = WarriorViewNames.ArabCulture

    override val cultureInfo: CultureInfo = CultureInfo(StateForm("caliphate", "republic"),
      List("Sivrikent",
        "Kagipinari",
        "Jirozeh",
        "Javahreza",
        "Az Zuyala",
        "Tall Hilladiyah",
        "Siwareya",
        "Ralkha",
        "Sharurma",
        "Dubasha",
        "As Sataq",
        "Ash Shiwit",
        "Arizah",
        "Amarraq",
        "Ovasi",
        "Çukupasa",
        "Latiabod",
        "Genaft",
        "Hadirnah",
        "Hadibayr",
        "Ismailqas",
        "Farayoum",
        "Jubahat",
        "Dhabail",
        "Zajibar",
        "Sa'dabar",
        "Damabi",
        "Qardalkrom",
        "Çanasun",
        "Inhiceli",
        "Bardalam",
        "Saralmas",
        "Barah",
        "An Nasiwr",
        "Abu Humein",
        "Senuroh",
        "Dhuhran",
        "Jalath",
        "Sahayya",
        "Say'uwkhah",
        "Al Suqameir",
        "Al Sisasiyah"),
      List("Rashidun",
        "Umayyad",
        "Abbasid",
        "Ushmaan",
        "Fatimid",
        "Almohad",
        "Sokoto",
        "Ramazan",
      )
    )
  }

  case object SpainHuman extends Culture("spanish", Humans, "humanHill", Color.Orange) {
    override val warriorViewNames: WarriorViewNames = WarriorViewNames.LuzCulture

    override val cultureInfo: CultureInfo = CultureInfo(StateForm("kingdom", "republic"),
      List("Panoschato",
        "Metakala",
        "Dralfeia",
        "Gammare",
        "Bergarsa",
        "Bagheria",
        "Louroso",
        "Bedes",
        "Almeires",
        "Laledo",
        "Almercia",
        "Corudorm",
        "Nafpargyroi",
        "Badine",
        "Agrine",
        "Rogola",
        "Covilves",
        "Liganca",
        "Senchal",
        "Blavera",
        "Hospidorm",
        "Ourecante",
        "Iliounghi",
        "Lepani",
        "Molfeto",
        "Parebaca",
        "Gandofeira",
        "Felgueisinde",
        "Corurotira",
        "Santanaria",
        "Madra",
        "Kosa",
        "Kilene",
        "Ligna",
        "Pesanevento",
        "Aveigoa",
        "Marivelas",
        "Valenresa",
        "Benilmas"),
      List("Asturian",
        "Valencian",
        "Aragonese",
        "Spanish",
        "Catalan",
        "Cuban",
        "Brazilian",
        "Mexican",
        "Balearic"))
  }

  case object WoodElves extends Culture(name = "woodElves", Elves, "elvenHouse", Color.LightGreen) {
    override val warriorViewNames: WarriorViewNames = WarriorViewNames.WoodElvesCulture

    override val cultureInfo: CultureInfo = CultureInfo(StateForm("lordship", "federation"),
      List("Asyhe Belanore",
        "Alm Taesi",
        "Galma Serine",
        "Unfadorei",
        "Sylnaneas",
        "Nyll Asari",
        "Nyneshys",
        "Thaanalume",
        "Thellion",
        "Inma Ancalen",
        "Thaath Entheas",
        "Lelaenanaes",
        "Eno Aethel",
        "Iyhotalos",
        "Famdorei",
        "Y'aenaluna",
        "Famemelle",
        "Yllha Asari",
        "Omyvenaes",
        "Wnebel",
        "Hyo Thalore",
        "Eshalsera",
        "Enn Allanar",
        "Asathsari",
        "Kefathemar",
        "Nyalnore",
        "Orobel",
        "Oshhshara",
        "Elyssari"),
      List("Quendian",
        "Eldarian",
        "Avarian",
        "Vanyarian",
        "Noldorian",
        "Telerian",
        "Falmarian",
        "Sindarian",
        "Nandorian"))
  }

  case object DesertElves extends Culture("desertElves", Elves, "elvenHouse", Color.LightYellow) {
    override val warriorViewNames: WarriorViewNames = WarriorViewNames.DesertElvesCulture

    override val cultureInfo: CultureInfo = CultureInfo(StateForm("lordship", "federation"),
      List(
        "Ellne Esari",
        "Mytren Themar",
        "Emyrengroth",
        "Shylvadell",
        "Gahshara",
        "Nenadorei",
        "Shaaenalin",
        "Nythrenlone",
        "Aseththaes",
        "Ylle Dorei",
        "Kseshara",
        "Iynehil",
        "Ami Shaeras",
        "Lella Dorei",
        "Mylmneas",
        "Selenserin",
        "Hyse Serin",
        "Inynlone",
        "Selfatheas",
        "Enyngroth",
        "Nytlathyr",
        "Inyf Anore",
        "Erfe Esari",
        "Yllnalona",
        "Eskadi",
        "Emyfmelle",
        "Canedell",
        "Anle Esari",
        "Onnesari",
        "Mef Caelora"
      ),
      List(
        "Unarithan",
        "Serinian",
        "Fasenorian",
        "Thylladoreian",
        "Alorian",
        "Ormedoreian",
        "Ullholuman",
        "Themarian"))
  }

  case object WesnothOrcs extends Culture("orcs", Orcs, "orcCity", Color.DarkRed) {

    override val warriorViewNames: WarriorViewNames = WarriorViewNames.WesnothOrcCulture

    override val cultureInfo: CultureInfo = CultureInfo(StateForm("horde", "confederation"),
      List(
        "Ukh Murdrerd",
        "Ez Noccokh",
        "Zorbrard",
        "Dashbag",
        "Laldrerd",
        "Bragvar",
        "Barkurkrir",
        "Dozdogan",
        "Drulgrudh Gigh",
        "Zrodkag Roz",
        "Kard Qalbin",
        "Derd Brodzokh",
        "Dhigun",
        "Bhurkron",
        "Gerzu",
        "Kodrord",
        "Dredrurdrodh",
        "Buccazzegh",
        "Dugnodh Udh",
        "Dulkal Ra",
        "On Vrorgir",
        "Bhadh Zrermad",
        "Vulgrud",
        "Zrodridh",
        "Irdog",
        "Lulgikh",
        "Dralgashnad",
        "Agrorbrod",
        "Ugol Ghedh",
        "Dhugga Vrod"
      ),
      List(
        "Black Tooth Grin",
        "Blackrock",
        "Bleeding Hollow",
        "Bonechewer",
        "Burning Blade",
        "Dragonmaw",
        "Frostwolf",
        "Laughing Skull",
        "Mok'Nathal",
        "Shadowmoon",
        "Shattered Hand",
        "Stormreaver",
        "Thunderlord"))
  }

  case object WesnothDwarfes extends Culture("dwarfs", Dwarfs, "dwarfCity", Color.White) {
    override val warriorViewNames: WarriorViewNames = WarriorViewNames.WesnothDwarfCulture
    override val cultureInfo: CultureInfo = CultureInfo(StateForm("clan", "confederation"),
      List(
        "Bag Thurum",
        "Khorn Boldihr",
        "Kamburim",
        "Girtorhm",
        "Vonladuhr",
        "Nig Faldur",
        "Morborimm",
        "Vagfarum",
        "Khon Lodahr",
        "Daluluhm",
        "Khigh Buldohr",
        "Kern Burim",
        "Dhanbadur",
        "Banbaduhr",
        "Thimfaruhm",
        "Gogh Buldahr",
        "Vel Garuhm",
        "Bam Kahldur",
        "Khir Darohm",
        "Khugboramm",
        "Dumkohldur",
        "Herdim",
        "Keltorum",
        "Han Thurim",
        "Thalulur",
        "Gigham",
        "Vandarim",
        "Mineduhr",
        "Bhighbuldohr",
        "Bontarum"
      ),
      List(
        "Khazad-dum",
        "Nogrod",
        "Belegost",
        "Iron Hills",
        "Ironfists",
        "Khaz Modan",
        "Wildhammer",
        "Grim Batol"
      ))
  }

  case object WesnothUndead extends Culture("undead", Undead, "caveCity", Color.Brown) {
    override val warriorViewNames: WarriorViewNames = WarriorViewNames.WesnothUndeadCulture
    override val cultureInfo: CultureInfo = CultureInfo(StateForm("hegemony", "union"),
      List(
        "Tugrad",
        "Juchuth",
        "Okrax",
        "Taltozkos",
        "Zanrormid",
        "Nixruuzur",
        "As Namkhok",
        "Kur Ukkaar",
        "Torquth Vass",
        "Phudux Phir",
        "Malrer",
        "Naxxox",
        "Zaurkhox",
        "Orkherrax",
        "Kaxnushix",
        "Kunkhoshzis",
        "Ux Ankar",
        "Khas Vixxrud",
        "Phoxxais Touss",
        "Mumkuss Khos",
        "Goshos",
        "Zubrud",
        "Zoxras",
        "Phunkihress",
        "Tulguggux",
        "Agzikzaz",
        "Chuus Olzik",
        "Iz Zocred",
        "Oggruss Khaiss",
        "Nushrek Ax"
      ),
      List(
        "Urkhissian",
        "Chankhaidan",
        "Nolturian",
        "Cholrairhathan",
        "Tecramossian",
        "Kounkharozian",
        "Ad Varqoxian",
        "Zoud Jexrarian",
        "Khiszis Kurian",
        "Toxruss Gourian"
      ))
  }

  private def scaleNeeds(needsToScale: CornerPopulationNeeds): CornerPopulationNeeds = {
    needsToScale.transform { case (popClass, classNeedsMap) =>
      classNeedsMap.transform { case (popNeedsType, needsMap) =>
        val q = needsQ(popClass)(popNeedsType)
        needsMap.scaleToSum(q)
      }
    }
  }

  private def defaultIlliterateHumanNeeds(culture: Culture): CornerPopulationNeeds = Map(
    Lower -> Map(
      LifeNeeds -> Map(Grain -> 2, Fruit -> 1, Cattle -> 1, Coal -> 1),
      RegularNeeds -> Map(Coal -> 1, Herbs -> 1, Tea -> 1, Magic -> 1, Cotton -> 1, Ritual(culture) -> 1),
      LuxuryNeeds -> Map(Iron -> 1, Wood -> 1, Magic -> 1, Ritual(culture) -> 1, Coal -> 1)
    ),
    Middle -> Map(
      LifeNeeds -> Map(Grain -> 2, Fruit -> 1, Cattle -> 1, Coal -> 1),
      RegularNeeds -> Map(Coal -> 1, Herbs -> 1, Coffee -> 1, Magic -> 1, Cotton -> 1, Ritual(culture) -> 1),
      LuxuryNeeds -> Map(PreciousMetal -> 1, Iron -> 1, Wood -> 1, Magic -> 1, Ritual(culture) -> 1, Coal -> 1)
    ),
    Upper -> Map(
      LifeNeeds -> Map(Grain -> 2, Fruit -> 1, Cattle -> 1, Coal -> 1),
      RegularNeeds -> Map(Coal -> 1, Herbs -> 1, Coffee -> 1, Magic -> 1, Cotton -> 1, Ritual(culture) -> 1),
      LuxuryNeeds -> Map(PreciousMetal -> 1, Iron -> 1, Wood -> 1, Magic -> 1, Ritual(culture) -> 1, Coal -> 1)
    )
  )

  private def defaultLiterateHumanNeeds(culture: Culture): CornerPopulationNeeds = Map(
    Lower -> Map(
      LifeNeeds -> Map(Grain -> 2, Wine -> 1, Liquor -> 1, Coal -> 1, Cattle -> 1),
      RegularNeeds -> Map(Coal -> 1, Medicine -> 1, Tea -> 1, Magic -> 1, Clothes -> 1, Ritual(culture) -> 1),
      LuxuryNeeds -> Map(Weapons -> 1, Furniture -> 1, Magic -> 1, Ritual(culture) -> 1, Coal -> 1)
    ),
    Middle -> Map(
      LifeNeeds -> Map(Grain -> 2, Wine -> 1, Liquor -> 1, Coal -> 1, Cattle -> 1),
      RegularNeeds -> Map(Coal -> 1, Medicine -> 1, Coffee -> 1, Magic -> 1, Clothes -> 1, Ritual(culture) -> 1),
      LuxuryNeeds -> Map(Jewelry -> 1, Weapons -> 1, Furniture -> 0.8, Paper -> 0.2, Magic -> 1, Ritual(culture) -> 1, Coal -> 1)
    ),
    Upper -> Map(
      LifeNeeds -> Map(Grain -> 2, Wine -> 1, Liquor -> 1, Coal -> 1, Cattle -> 1),
      RegularNeeds -> Map(Coal -> 1, Medicine -> 1, Coffee -> 1, Magic -> 1, Clothes -> 1, Ritual(culture) -> 1),
      LuxuryNeeds -> Map(Jewelry -> 1, Weapons -> 1, Furniture -> 0.8, Paper -> 0.2, Magic -> 1, Ritual(culture) -> 1, Coal -> 1)
    )
  )
}

abstract class Culture(val name: String, val race: Race, val houseStyle: String, val color: Color) {

  import Culture._

  def needs: PopulationNeeds = PopulationNeeds(
    scaleNeeds(defaultIlliterateHumanNeeds(this)),
    scaleNeeds(defaultLiterateHumanNeeds(this)))

  def cultureNameKey: String = "culture." + name

  val warriorViewNames: WarriorViewNames

  val cultureInfo: CultureInfo
}
