package mr.merc.economics

object Products {
  val AllProducts:List[Product] = List(
    Grain, Fruit, Cattle, Tea, Coffee, Cotton, Herbs, Coal, Wood, Iron, PreciousMetal,
    Magic, Medicine, Jewelry, Wine, Paper, Furniture, Liquor, Clothes, Weapons,
  ) ++ Culture.cultures.map(Ritual.apply)

  val IndustryProducts:List[IndustryProduct] = AllProducts.collect {
    case x:IndustryProduct => x
  }

  def productByName(name: String): Product = AllProducts.find(_.toString.toLowerCase == name.toLowerCase).getOrElse(
    sys.error(s"Didn't find product $name in $AllProducts"))

  sealed abstract class Product extends scala.Product with Serializable {
    def name: String = productPrefix
  }
  abstract class GatheredProduct extends Product
  abstract class FarmProduct extends GatheredProduct
  abstract class ResourceProduct extends GatheredProduct

  abstract class IndustryProduct(goodsToProduce:(Product, Double)*) extends Product {
    require(goodsToProduce.nonEmpty, "Components can't be empty!")

    val components:Map[Product, Double] = goodsToProduce.toMap
  }

  // farms
  // food
  case object Grain extends FarmProduct
  case object Fruit extends FarmProduct
  case object Cattle extends FarmProduct

  // drinks
  case object Tea extends FarmProduct
  case object Coffee extends FarmProduct

  // narcotics
  //case object Opium extends FarmProduct

  // for clothes production
  case object Cotton extends FarmProduct

  // for medicine
  case object Herbs extends FarmProduct

  // mines and forest
  case object Wood extends ResourceProduct
  case object Coal extends ResourceProduct
  case object Iron extends ResourceProduct
  case object PreciousMetal extends ResourceProduct

  // churches
  case class Ritual(culture: Culture) extends ResourceProduct {
    override def name: String = culture.name + " " + super.name
  }

  // magic products
  case object Magic extends ResourceProduct

  // factories
  case object Medicine extends IndustryProduct(Herbs -> 1)
  case object Jewelry extends IndustryProduct(PreciousMetal -> 1)
  case object Weapons extends IndustryProduct(Iron -> 1)
  case object Liquor extends IndustryProduct(Grain -> 1)
  case object Clothes extends IndustryProduct(Cotton -> 1)
  case object Wine extends IndustryProduct(Fruit -> 1)
  case object Paper extends IndustryProduct(Wood -> 1)
  case object Furniture extends IndustryProduct(Wood -> 1)

  // products to add: sulphur, fertilizer,fish, boots/shoes, horses

}


