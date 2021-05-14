package mr.merc.unit.view

import mr.merc.image.MImage
import mr.merc.view.move.LinearMovement
import mr.merc.view.move.Movement
import mr.merc.view.Sprite
import mr.merc.view.SpriteState
import scalafx.scene.canvas.GraphicsContext
import mr.merc.sound.Sound
import mr.merc.map.hex.view.TerrainHexView

class ProjectileView(start: Option[List[MImage]], move: Option[List[MImage]], end: Option[List[MImage]],
                     from: (Int, Int), to: (Int, Int), speed: Int, factor: Double, sounds: Map[ProjectileSoundState, Sound]) extends Sprite[ProjectileState](
  Map(ProjectileNotStarted -> List(MImage.emptyImage),
    ProjectileStart -> start.getOrElse(List(MImage.emptyImage)),
    ProjectileMovement -> move.getOrElse(List(MImage.emptyImage)),
    ProjectileEnd -> end.getOrElse(List(MImage.emptyImage)),
    ProjectileFinished -> List(MImage.emptyImage)),
  ProjectileNotStarted, factor, mirroringEnabled = false) {

  val movement = new LinearMovement(from._1, from._2, to._1, to._2, speed)
  movement.start()
  coords = (from._1, from._2)
  centered = Some(TerrainHexView.side(factor))

  override def state_=(st: ProjectileState) {
    super.state_=(st)
    val soundState = st match {
      case ProjectileStart => Some(ProjectileStartSound)
      case ProjectileEnd => Some(ProjectileEndSound)
      case ProjectileMovement => Some(ProjectileMoveStartSound)
      case ProjectileFinished | ProjectileNotStarted => None
    }

    soundState flatMap (s => sounds.get(s)) foreach (_.play())
  }

  override def updateTime(time: Int): Int = {
    val result = super.updateTime(time)

    if (state == ProjectileMovement) {
      movement.update(time)
      coords = (movement.x, movement.y)
    }

    if (result != 0 && index == 0 || movement.isOver && this.state == ProjectileMovement) {
      state match {
        case ProjectileStart =>
          if (move.isDefined) {
            this.state = ProjectileMovement
          } else if (end.isDefined) {
            this.state = ProjectileEnd
            coords = to
          } else {
            this.state = ProjectileFinished
          }

        case ProjectileMovement =>
          if (movement.isOver) {
            if (end.isDefined) {
              this.state = ProjectileEnd
            } else {
              this.state = ProjectileFinished
            }
          }
        case ProjectileEnd => this.state = ProjectileFinished
        case ProjectileNotStarted | ProjectileFinished => // do nothing in this case
      }
    }

    result
  }
}

sealed trait ProjectileState extends SpriteState

object ProjectileStart extends ProjectileState

object ProjectileMovement extends ProjectileState

object ProjectileEnd extends ProjectileState

object ProjectileFinished extends ProjectileState

object ProjectileNotStarted extends ProjectileState