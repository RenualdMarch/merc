package mr.merc.unit.view

case class AttackView(index: Int, imageName: String, projectile: Option[String] = None) {
  def projectileName(success: Boolean) = success match {
    case true => projectile map (_ + "-succ")
    case false => projectile map (_ + "-fail")
  }
}