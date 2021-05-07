package mr.merc.economics

import mr.merc.army.WarriorViewNames
import mr.merc.economics.Population._
import mr.merc.economics.Products._
import mr.merc.economics.WorldConstants.Population.needsQ
import scalafx.scene.paint.Color
import MapUtil.FloatOperations._

object Culture {

  private val humanCultures = List(LatinHuman, FrenchHuman, DarkHuman, GreekHuman, GermanHuman, ArabHuman, SlavicHuman, SpainHuman)

  private val elvishCultures = List(WoodElves, DesertElves, DarkElves)

  private val orcishCultures = List(WesnothOrcs, LaticOrcs)

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

  case class StateForm(monarchy: String, democracy: String, monarchyTitle: String, democracyTitle: String)

  case class CultureInfo(stateForm: StateForm, cities: List[String], states: List[String],
                         maleNames: List[String])

  case object LatinHuman extends Culture("latin", Humans, "humanCity", Color.Red) {
    val warriorViewNames: WarriorViewNames = WarriorViewNames.LatinCulture

    val cultureInfo: CultureInfo = CultureInfo(StateForm("empire", "republic", "emperor", "consul"),
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
        "Lux"),
      List("Augustus",
        "Tiberius",
        "Caesar",
        "Claudius",
        "Marcus",
        "Lucius",
        "Antonius",
        "Domitian",
        "Valerian",
        "Gordian"))
  }

  case object FrenchHuman extends Culture("french", Humans, "humanVillage", Color.Blue) {
    val warriorViewNames: WarriorViewNames = WarriorViewNames.WesnothCulture

    val cultureInfo: CultureInfo = CultureInfo(StateForm("kingdom", "republic", "king", "president"),
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
        "Burgundian",
        "French"),
      List("Henry",
        "Louis",
        "Charles",
        "Francis",
        "John",
        "Philip"))
  }

  case object DarkHuman extends Culture("nilf", Humans, "humanCottage", Color.Black) {
    val warriorViewNames: WarriorViewNames = WarriorViewNames.DarkHumanCulture

    val cultureInfo: CultureInfo = CultureInfo(StateForm("empire", "republic", "emperor", "president"),
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
        "Ublanian"),
      List("Anahid",
        "Ardal",
        "Cahir",
        "Declan",
        "Ceallach",
        "Emhyr",
        "Havart"
      ))
  }

  case object GreekHuman extends Culture("greek", Humans, "humanCity", Color.Gray) {
    val warriorViewNames: WarriorViewNames = WarriorViewNames.GreekDardoCulture

    val cultureInfo: CultureInfo = CultureInfo(StateForm("kingdom", "republic", "king", "president"),
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
        "Macedonian",
        "Greek",
        "Beotian",
        "Trojan",
        "Achaean",
        "Corinthian"),
      List("Alexander",
        "Perdikkas",
        "Philip",
        "Pyrrhos",
        "Antigonos",
        "Lysimachos",
        "Demetrios",
        "Ptolemy",
        "Antiochos",
        "Mithridates"
      ))
  }

  case object GermanHuman extends Culture("german", Humans, "humanVillage", Color.Yellow) {
    val warriorViewNames: WarriorViewNames = WarriorViewNames.ChevalierCulture

    val cultureInfo: CultureInfo = CultureInfo(StateForm("empire", "republic", "emperor", "сhancellor"),
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
        "Prussian"),
      List("Wilhelm",
        "Friedrich",
        "Ferdinand",
        "Eugene",
        "Karl",
        "Leopold",
        "Joseph",
        "Charles",
        "Albert"
      ))
  }

  case object SlavicHuman extends Culture("slavic", Humans, "humanHut", Color.Violet) {
    val warriorViewNames: WarriorViewNames = WarriorViewNames.WolfCulture

    val cultureInfo: CultureInfo = CultureInfo(StateForm("principality", "republic", "prince", "president"),
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
        "Podolian"),
      List("Oleg",
        "Igor",
        "Sviatoslav",
        "Yaropolk",
        "Vladimir",
        "Yaroslav",
        "Iziaslav",
        "Vsevolod",
        "Yuri",
        "Rostislav",
        "Mstislav"
      ))
  }

  case object ArabHuman extends Culture("arab", Humans, "humanCottage", Color.Green) {
    override val warriorViewNames: WarriorViewNames = WarriorViewNames.ArabCulture

    override val cultureInfo: CultureInfo = CultureInfo(StateForm("caliphate", "republic", "caliph", "president"),
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
      ),
      List(
        "Abu Bakr",
        "Umar",
        "Ali",
        "Hasan",
        "Hussein",
        "Abbas",
        "Harun",
        "Ahmad",
        "Mahmud"
      )
    )
  }

  case object SpainHuman extends Culture("spanish", Humans, "humanHill", Color.Orange) {
    override val warriorViewNames: WarriorViewNames = WarriorViewNames.LuzCulture

    override val cultureInfo: CultureInfo = CultureInfo(StateForm("kingdom", "republic", "king", "president"),
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
        "Balearic"),
      List(
        "Felipe",
        "Juan",
        "Carlos",
        "Alfonso",
        "Fernando",
        "Jose",
        "Francisco"
      ))
  }

  case object WoodElves extends Culture(name = "woodElves", Elves, "elvenHouse", Color.LightGreen) {
    override val warriorViewNames: WarriorViewNames = WarriorViewNames.WoodElvesCulture

    override val cultureInfo: CultureInfo = CultureInfo(StateForm("lordship", "federation", "lord", "secretary"),
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
        "Nandorian"),
      List(
        "Aenarion",
        "Caledor",
        "Tethlis",
        "Aethis",
        "Elrond",
        "Thranduil",
        "Celebrimbor",
        "Beleriand",
        "Aman"
      ))
  }

  case object DesertElves extends Culture("desertElves", Elves, "elvenHouse", Color.LightYellow) {
    override val warriorViewNames: WarriorViewNames = WarriorViewNames.DesertElvesCulture

    override val cultureInfo: CultureInfo = CultureInfo(StateForm("lordship", "federation", "lord", "secretary"),
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
        "Themarian"),
      List(
        "Aldon",
        "Cirdan",
        "Earendil",
        "Elladan",
        "Gildor",
        "Haldir",
        "Halafarin",
        "Othorion"
      )
    )
  }

  case object WesnothOrcs extends Culture("orcs", Orcs, "orcCity", Color.DarkRed) {

    override val warriorViewNames: WarriorViewNames = WarriorViewNames.WesnothOrcCulture

    override val cultureInfo: CultureInfo = CultureInfo(StateForm("horde", "confederation", "chief", "president"),
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
        "Thunderlord"),
      List("Supaugh",
        "Fidgug",
        "Uzul",
        "Naguk",
        "Sugha",
        "Orgha",
        "Karthurg",
        "Vargan",
        "Zulmthu",
        "Sogugbu"))
  }

  case object LaticOrcs extends Culture("latinOrcs", Orcs, "orcCity", Color.Pink) {

    override val warriorViewNames: WarriorViewNames = WarriorViewNames.LatinOrcCulture

    override val cultureInfo: CultureInfo = CultureInfo(StateForm("empire", "republic", "emperor", "consul"),
      List(
        "Iz Zredka",
        "Bhord Mudzard",
        "Dhadgral",
        "Kruzrodh",
        "Bezgal",
        "Korgrur",
        "Lugholgrird",
        "Krummoror",
        "Brolbrudh Ded",
        "Mugnagh Brukh",
        "Ver Krada",
        "Kredh Muzigh",
        "Olgrugh",
        "Drobridh",
        "Brilzu",
        "Gagmerd",
        "Arduccugh",
        "Uggoggard",
        "Ugad Og",
        "Razdron Brudh",
        "Uz Bherbro",
        "Bhadh Nigedh",
        "Grocrun",
        "Vredrodh",
        "Zrulkug",
        "Odga",
        "Goddugard",
        "Gerkrulgrodh",
        "Garig Chudh",
        "Liggan Gokh"
      ),
      List(
        "Zar Bharrodh",
        "Drakh Kredgrul",
        "Lardrug",
        "Chorir",
        "Dholgrag",
        "Vizgu",
        "Grammiggiz",
        "Chirgoldrur",
        "Zrugrogh Zral",
        "Grubrerd Dodh"
      ),
      List(
        "Zaghig",
        "Umug",
        "Kabugbu",
        "Shagol",
        "Magdud",
        "Narhbub",
        "Jregh",
        "Jokgagu",
        "Rugdumph",
        "Toghat"
      )
    )
  }

  case object WesnothDwarfes extends Culture("dwarfs", Dwarfs, "dwarfCity", Color.White) {
    override val warriorViewNames: WarriorViewNames = WarriorViewNames.WesnothDwarfCulture
    override val cultureInfo: CultureInfo = CultureInfo(StateForm("clan", "confederation", "chieftain", "сhancellor"),
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
      ),
      List(
        "Salmumin",
        "Brafraeg",
        "Dunan",
        "Bamur",
        "Balor",
        "Dwokhumlin",
        "Malgon",
        "Glorgan",
        "Dilur",
        "Ralvol",
        "Dranan",
      ))
  }

  case object WesnothUndead extends Culture("undead", Undead, "caveCity", Color.Brown) {
    override val warriorViewNames: WarriorViewNames = WarriorViewNames.WesnothUndeadCulture
    override val cultureInfo: CultureInfo = CultureInfo(StateForm("hegemony", "union", "magus", "secretary"),
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
      ),
      List(
        "Pheex'zis",
        "Czauk'shux",
        "Xhuxeak",
        "Jog'voq",
        "Dhuarrel",
        "Ath'gez",
        "Pit'qoucic",
        "Uc'zurad",
        "Bod'zulas",
        "Bamzugruq",
      ))
  }

  case object DarkElves extends Culture("darkElves", Elves, "elvenHouse", Color.DarkGray) {
    override val warriorViewNames: WarriorViewNames = WarriorViewNames.DarkElvesCulture
    override val cultureInfo: CultureInfo = CultureInfo(StateForm("dynasty", "confederation", "patriarch", "secretary"),
      List(
        "Omyne Elunore",
        "Irha Shaeras",
        "Osyfe Serine",
        "Merannoris",
        "Lelfserin",
        "Amyve Nalore",
        "Enan Esari",
        "Asntalos",
        "Iysa Anore",
        "Amyo Serin",
        "Mythnqua",
        "Oshan Nalore",
        "Waholume",
        "Ylaenasari",
        "Cal Anore",
        "Asethbelle",
        "Jaathlean",
        "Amyfa Unarith",
        "Sylho Esari",
        "Shehomelle",
        "Thavalenor",
        "Alrannore",
        "Myfelune",
        "Sylorius",
        "Neeth Elunore",
        "Iyhe Dorei",
        "Emrenrion",
        "Selirion",
        "Syhlenora",
        "Milvaserine"
      ),
      List(
        "Teir'Dal",
        "Midkemia",
        "Krondor",
        "Kryn",
        "Xhorhas",
        "Urzin",
        "Dranas"
      ),
      List(
        "Сukliorn",
        "Ciknimi",
        "Brakril",
        "Giomodh",
        "Chepasce",
        "Ghumnal",
        "Imnuro",
        "Bhelushru",
        "Grevrael",
        "Dhanicsu",
      )
    )
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
      LifeNeeds -> Map(Grain -> 2, Fruit -> 1, Cattle -> 1, Coal -> 1, Liquor -> 1),
      RegularNeeds -> Map(Coal -> 1, Herbs -> 1, Tea -> 1, Magic -> 1, Cotton -> 1, Ritual(culture) -> 1),
      LuxuryNeeds -> Map(Iron -> 1, Wood -> 1, Magic -> 1, Ritual(culture) -> 1, Coal -> 1)
    ),
    Middle -> Map(
      LifeNeeds -> Map(Grain -> 2, Fruit -> 1, Cattle -> 1, Coal -> 1, Liquor -> 1),
      RegularNeeds -> Map(Coal -> 1, Herbs -> 1, Coffee -> 1, Magic -> 1, Cotton -> 1, Ritual(culture) -> 1),
      LuxuryNeeds -> Map(PreciousMetal -> 1, Iron -> 1, Wood -> 1, Magic -> 1, Ritual(culture) -> 1, Coal -> 1)
    ),
    Upper -> Map(
      LifeNeeds -> Map(Grain -> 2, Fruit -> 1, Cattle -> 1, Coal -> 1, Liquor -> 1),
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

  import mr.merc.util.MercUtils._
  def randomMaleName: String = cultureInfo.maleNames.randomElement()
}
