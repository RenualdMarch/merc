package mr.merc.economics

object Products {
  val ProductTradingRounds:List[List[Product]] = List(
    List(Grain, Fish, Fruit, Cattle, Tea, Coffee, Opium, Cotton, Herbs, Timber, Coal, Iron, Sulphur),
    List(Lumber, Cement, Fabric, Fertilizer, Paper, Glass, Steel, Amulet, Medicine),
    List(Furniture, Liquor, Clothes, Wine, Weapons, MachineParts)
  )

  val AllProducts:List[Product] = ProductTradingRounds.flatten


  sealed abstract class Product
  sealed abstract class FarmProduct extends Product
  sealed abstract class ResourceProduct extends Product

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

  // factories
  // simple
  case object Lumber extends IndustryProduct(Timber -> 3)
  case object Cement extends IndustryProduct(Coal -> 3)
  case object Fabric extends IndustryProduct(Cotton -> 3)
  case object Fertilizer extends IndustryProduct(Sulphur -> 3)
  case object Paper extends IndustryProduct(Timber -> 3)
  case object Glass extends IndustryProduct(Coal -> 3)
  case object Steel extends IndustryProduct(Iron -> 3)

  // complex
  case object Furniture extends IndustryProduct(Lumber -> 1)
  case object Liquor extends IndustryProduct(Glass -> 0.4, Grain -> 0.6)
  case object Clothes extends IndustryProduct(Fabric -> 1)
  case object Wine extends IndustryProduct(Glass -> 0.4, Fruit -> 0.6)
  case object Weapons extends IndustryProduct(Steel -> 1)
  case object MachineParts extends IndustryProduct(Coal -> 0.9, Steel -> 0.7)

  // magic products
  case object Amulet extends MagicProduct(Timber -> 3)
  case object Medicine extends MagicProduct(Herbs -> 3)

  // products to add: boots/shoes, horses, books

  val PeoplePerOneFactoryLevel = 1000
  val FactoryOutputMultiplier =  20
  val GatheringOutputMultiplier = 20
}


