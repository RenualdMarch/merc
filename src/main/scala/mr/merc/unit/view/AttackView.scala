package mr.merc.unit.view

import mr.merc.image.MImage
import scalafx.scene.image.Image

case class AttackView(index: Int, imageName: String, projectile: Option[String] = None) {
  def projectileName(success: Boolean) = success match {
    case true => projectile map (_ + "-succ")
    case false => projectile map (_ + "-fail")
  }

  def attackImage:Image = MImage("/images/attacks/" + imageName + ".png").image
}