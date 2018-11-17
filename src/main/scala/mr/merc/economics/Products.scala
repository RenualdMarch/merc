package mr.merc.economics

import mr.merc.economics.Population.Culture

object Products {
  val AllProducts:List[Product] = List(
    Grain, Fish, Fruit, Cattle, Tea, Coffee, Opium, Cotton, Herbs, Timber, Coal, Iron, Sulphur,
    Lumber, Cement, Fabric, Fertilizer, Paper, Glass, Steel, Amulet, Medicine,
    Furniture, Liquor, Clothes, Wine, Weapons, MachineParts, PreciousMetal
  ) ++ Population.cultures.map(Ritual.apply)

  def productByName(name: String): Product = AllProducts.find(_.toString.toLowerCase == name.toLowerCase).getOrElse(
    sys.error(s"Didn't find product $name in $AllProducts"))

  sealed abstract class Product extends scala.Product with Serializable {
    def name: String = productPrefix
  }
  sealed abstract class GatheredProduct extends Product
  sealed abstract class FarmProduct extends GatheredProduct
  sealed abstract class ResourceProduct extends GatheredProduct

  sealed abstract class ProducibleProduct(val components:Map[Product, Double]) extends Product {
    require(components.nonEmpty, "Components can't be empty!")
  }

  sealed abstract class IndustryProduct(goodsToProduce:(Product, Double)*) extends ProducibleProduct(goodsToProduce.toMap)

  sealed abstract class MagicProduct(goodsToProduce:(Product, Double)*) extends ProducibleProduct(goodsToProduce.toMap)

  // farms
  // food
  case object Grain extends FarmProduct
  case object Fish extends FarmProduct
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
  case object Sulphur extends ResourceProduct

  // churches
  case class Ritual(culture: Culture) extends ResourceProduct

  // factories
  // simple
  case object Lumber extends IndustryProduct(Timber -> 1)
  case object Cement extends IndustryProduct(Coal -> 1)
  case object Fabric extends IndustryProduct(Cotton -> 1)
  case object Fertilizer extends IndustryProduct(Sulphur -> 1)
  case object Paper extends IndustryProduct(Timber -> 1)
  case object Glass extends IndustryProduct(Coal -> 1)
  case object Steel extends IndustryProduct(Iron -> 1)

  // complex
  case object Furniture extends IndustryProduct(Lumber -> 1)
  case object Liquor extends IndustryProduct(Glass -> 0.4, Grain -> 0.6)
  case object Clothes extends IndustryProduct(Fabric -> 1)
  case object Wine extends IndustryProduct(Glass -> 0.4, Fruit -> 0.6)
  case object Weapons extends IndustryProduct(Steel -> 1)
  case object MachineParts extends IndustryProduct(Coal -> 0.4, Steel -> 0.6)

  // magic products
  case object Amulet extends MagicProduct(Timber -> 1)
  case object Medicine extends MagicProduct(Herbs -> 1)

  // products to add: boots/shoes, horses, books

  val PeoplePerOneFactoryLevel = 1000
}


