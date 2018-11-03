package mr.merc.ui.battle

import scalafx.Includes._
import mr.merc.map.hex.TerrainHex
import mr.merc.unit.Soldier
import scalafx.stage.Stage
import scalafx.scene.layout.VBox
import scalafx.scene.Scene
import scalafx.scene.control.TableView
import scalafx.scene.layout.HBox
import scalafx.scene.control.Button
import mr.merc.local.Localization
import scalafx.geometry.Pos
import scalafx.event.ActionEvent
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.TableColumn
import scalafx.beans.property.StringProperty
import scalafx.beans.property.ObjectProperty
import mr.merc.unit.Attack
import scalafx.scene.control.TableView.TableViewSelectionModel
import scalafx.beans.property.DoubleProperty
import mr.merc.unit.ChanceOfSuccess
import scalafx.scene.control.TableCell
import scalafx.scene.image.ImageView
import scalafx.scene.control.ContentDisplay
import mr.merc.image.MImage
import mr.merc.unit.AttackAttribute
import mr.merc.ai.AttackSelectionHelper
import mr.merc.unit.view.SoldierTypeViewInfo

class AttackSelectionDialog(attacker: Soldier, defender: Soldier, attackerHex: TerrainHex,
  defenderHex: TerrainHex) extends Stage {

  title = Localization("attack.dialog.title")
  var selectedAttack: Option[Attack] = None

  private case class AttackChoice(image: String, damage: Int, count: Int, chance: ChanceOfSuccess, attributes: Set[AttackAttribute])
  private type AttackPair = (AttackChoice, Option[AttackChoice])

  private val data = new ObservableBuffer[AttackPair]()
  data ++= attacks(attacker, defender, attackerHex, defenderHex)
  private val tableWidth = DoubleProperty(0)

  private val table = new TableView[AttackPair](data) {
    tableColumns foreach (c => columns += c)
    selectionModel.value.cellSelectionEnabled = false
    val bestAttack = AttackSelectionHelper.selectBestAttack(attacker, defender, attackerHex, defenderHex)
    selectionModel.value.select(bestAttack)
  }

  tableWidth <== table.width

  val okButton = new Button {
    text = Localization("common.ok")
    onAction = { e: ActionEvent =>
      val index = table.delegate.getSelectionModel().getSelectedIndex()
      selectedAttack = Some(attacker.soldierType.attacks(index))
      AttackSelectionDialog.this.close()
    }
  }
  val cancelButton = new Button {
    text = Localization("common.cancel")
    onAction = { e: ActionEvent =>
      AttackSelectionDialog.this.close()
    }
  }

  this.scene = new Scene {
    stylesheets.add("/css/attackSelectionDialog.css")
    content = new VBox {
      children = List(table, new HBox() {
        children = List(okButton, cancelButton)
        alignment = Pos.CenterRight

      })
    }
  }

  this.width = 700
  table.prefWidth <== 900

  private def tableColumns: List[TableColumn[AttackPair, _]] = {

    val attackersAttackColumn = new TableColumn[AttackPair, Option[String]] {
      text = Localization("attack.attack")
      cellValueFactory = { c =>
        val name = c.value._1.image
        if (name == "") {
          ObjectProperty(None)
        } else {
          ObjectProperty(Some(attackImagePath(name)))
        }
      }
      cellFactory = imageCellFactory _
      prefWidth = 60
    }

    val attackersDamageColumn = new TableColumn[AttackPair, String] {
      text = Localization("attack.damage")
      cellValueFactory = { c =>
        val str = c.value._1.damage + " * " + c.value._1.count
        StringProperty(str)
      }
      prefWidth <== tableWidth / 6 - 21
    }

    val attackersAttributesColumn = new TableColumn[AttackPair, String] {
      text = Localization("attack.attributes")
      cellValueFactory = { c =>
        val str = c.value._1.attributes.map(_.localizedName).mkString(", ")
        StringProperty(str)
      }
      prefWidth <== tableWidth / 6 - 20
    }

    val attackersChanceColumn = new TableColumn[AttackPair, String] {
      text = Localization("attack.chance")
      cellValueFactory = { c =>
        val str = c.value._1.chance.chanceNumber + "%"
        StringProperty(str)
      }
      prefWidth <== tableWidth / 6 - 21
    }

    val defendersAttackColumn = new TableColumn[AttackPair, Option[String]] {
      text = Localization("attack.attack")
      cellValueFactory = { c =>
        val path = c.value._2.map(_.image).map(attackImagePath)
        ObjectProperty(path)
      }
      cellFactory = imageCellFactory _
      prefWidth = 60
    }

    val defendersDamageColumn = new TableColumn[AttackPair, String] {
      text = Localization("attack.damage")
      cellValueFactory = { c =>
        val str = c.value._2 match {
          case Some(choice) => choice.damage + " * " + choice.count
          case None => "0 * 0"
        }
        StringProperty(str)
      }
      prefWidth <== tableWidth / 6 - 21
    }

    val defendersChanceColumn = new TableColumn[AttackPair, String] {
      text = Localization("attack.chance")
      cellValueFactory = { c =>
        val str = c.value._2.map(_.chance.chanceNumber).getOrElse("0") + "%"
        StringProperty(str)
      }
      prefWidth <== tableWidth / 6 - 20
    }

    val defendersAttributesColumn = new TableColumn[AttackPair, String] {
      text = Localization("attack.attributes")
      cellValueFactory = { c =>
        val str = c.value._2.map(_.attributes).getOrElse(Set()).map(_.localizedName).mkString(", ")
        StringProperty(str)
      }
      prefWidth <== tableWidth / 6 - 21
    }

    List(attackersAttackColumn, attackersDamageColumn, attackersAttributesColumn, attackersChanceColumn,
      defendersAttackColumn, defendersDamageColumn, defendersAttributesColumn, defendersChanceColumn)
  }

  private def attacks(attacker: Soldier, defender: Soldier, attackerHex: TerrainHex,
    defenderHex: TerrainHex): List[AttackPair] = {
    val attackersAttackViews = SoldierTypeViewInfo(attacker.soldierType.name).attacks
    val defendersAttackViews = SoldierTypeViewInfo(defender.soldierType.name).attacks

    val attacks = attacker.soldierType.attacks
    attacks map (a => {
      val defendersAttackOpt = Attack.selectBestAttackForDefender(attacker, defender, a)
      val attackDamage = Attack.possibleAttackersDamage(true, attacker, defender, a, defendersAttackOpt)
      val attackesChance = a.chanceOfSuccess(Attack.calculateSoldierDefence(defender, defenderHex))

      val imageName = attackersAttackViews.find(a.index == _.index).get.imageName
      val attackerChoice = AttackChoice(imageName, attackDamage, a.count, attackesChance, a.attributes)
      defendersAttackOpt match {
        case Some(defendersAttack) => {
          val defenderDamage = Attack.possibleAttackersDamage(false, defender, attacker, defendersAttack, Some(a))
          val defenderChance = defendersAttack.chanceOfSuccess(Attack.calculateSoldierDefence(attacker, attackerHex))
          val defenderImageName = defendersAttackViews.find(defendersAttack.index == _.index).get.imageName
          val defenderChoice = AttackChoice(defenderImageName, defenderDamage, defendersAttack.count,
            defenderChance, defendersAttack.attributes)
          (attackerChoice, Some(defenderChoice))
        }
        case None => {
          (attackerChoice, None)
        }
      }
    })
  }

  private def imageCellFactory(col: TableColumn[AttackPair, Option[String]]): TableCell[AttackPair, Option[String]] = {
    new TableCell[AttackPair, Option[String]](new javafx.scene.control.TableCell[AttackPair, Option[String]]() {
      val imageview = new ImageView()
      imageview.fitHeight = 60
      imageview.fitWidth = 60
      setContentDisplay(ContentDisplay.GraphicOnly)
      setGraphic(imageview)

      override def updateItem(itemOpt: Option[String], empty: Boolean) {
        super.updateItem(itemOpt, empty)
        if (!empty) {
          itemOpt match {
            case Some(item) => imageview.image = MImage(item).image
            case None => imageview.image = MImage.emptyImage.image
          }

        }
      }
    })
  }

  private def attackImagePath(name: String) = "/images/attacks/" + name + ".png"
}