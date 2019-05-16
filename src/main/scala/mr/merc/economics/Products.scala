package mr.merc.economics

object Products {
  val AllProducts:List[Product] = List(
    Grain, Fruit, Cattle, Tea, Coffee, Opium, Cotton, Herbs, Timber, Coal, Iron,
    Lumber, Cement, Fabric, Jewelry, Paper, Glass, Steel, Magic, Medicine,
    Furniture, Liquor, Clothes, Wine, Weapons, PreciousMetal
  ) ++ Culture.cultures.map(Ritual.apply)

  val IndustryProducts:List[IndustryProduct] = AllProducts.collect {
    case x:IndustryProduct => x
  }

  def productByName(name: String): Product = AllProducts.find(_.toString.toLowerCase == name.toLowerCase).getOrElse(
    sys.error(s"Didn't find product $name in $AllProducts"))

  sealed abstract class Product extends scala.Product with Serializable {
    def name: String = productPrefix
  }
  sealed abstract class GatheredProduct extends Product
  sealed abstract class FarmProduct extends GatheredProduct
  sealed abstract class ResourceProduct extends GatheredProduct

  sealed abstract class IndustryProduct(goodsToProduce:(Product, Double)*) extends Product {
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
  case object Opium extends FarmProduct

  // for clothes production
  case object Cotton extends FarmProduct

  // for medicine
  case object Herbs extends FarmProduct

  // mines and forest
  case object Timber extends ResourceProduct
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
  // simple
  case object Lumber extends IndustryProduct(Timber -> 1)
  case object Cement extends IndustryProduct(Coal -> 1)
  case object Fabric extends IndustryProduct(Cotton -> 1)
  case object Paper extends IndustryProduct(Timber -> 1)
  case object Glass extends IndustryProduct(Coal -> 1)
  case object Steel extends IndustryProduct(Iron -> 1)
  case object Medicine extends IndustryProduct(Herbs -> 1)
  case object Jewelry extends IndustryProduct(PreciousMetal -> 1)

  // complex
  case object Furniture extends IndustryProduct(Lumber -> 1)
  case object Liquor extends IndustryProduct(Glass -> 0.4, Grain -> 0.6)
  case object Clothes extends IndustryProduct(Fabric -> 1)
  case object Wine extends IndustryProduct(Glass -> 0.4, Fruit -> 0.6)
  case object Weapons extends IndustryProduct(Steel -> 1)


  // products to add: sulphur, fertilizer,MachineParts,fish, boots/shoes, horses, books

}


