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

  sealed abstract class FactoryProduct(goodsToProduce:(Product, Double)*) extends Product {
    val components:Map[Product, Double] = goodsToProduce.toMap
    require(components.nonEmpty, "Components can't be empty!")
  }

  sealed abstract class MagicProduct(goodsToProduce:(Product, Double)*) extends Product {
    val components:Map[Product, Double] = goodsToProduce.toMap
    require(components.nonEmpty, "Components can't be empty!")
  }

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
  case object Lumber extends FactoryProduct(Timber -> 3)
  case object Cement extends FactoryProduct(Coal -> 3)
  case object Fabric extends FactoryProduct(Cotton -> 3)
  case object Fertilizer extends FactoryProduct(Sulphur -> 3)
  case object Paper extends FactoryProduct(Timber -> 3)
  case object Glass extends FactoryProduct(Coal -> 3)
  case object Steel extends FactoryProduct(Iron -> 3)

  // complex
  case object Furniture extends FactoryProduct(Lumber -> 1)
  case object Liquor extends FactoryProduct(Glass -> 0.4, Grain -> 0.6)
  case object Clothes extends FactoryProduct(Fabric -> 1)
  case object Wine extends FactoryProduct(Glass -> 0.4, Fruit -> 0.6)
  case object Weapons extends FactoryProduct(Steel -> 1)
  case object MachineParts extends FactoryProduct(Coal -> 0.9, Steel -> 0.7)

  // magic products
  case object Amulet extends MagicProduct(Timber -> 3)
  case object Medicine extends MagicProduct(Herbs -> 3)

  // products to add: boots/shoes, horses, books

}


