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

class AttackSelectionDialog(attacker: Soldier, defender: Soldier, attackerHex: TerrainHex,
  defenderHex: TerrainHex) extends Stage {

  var selectedAttack: Option[Attack] = None

  private case class AttackChoice(image: String, damage: Int, count: Int, chance: Int)
  private type AttackPair = (AttackChoice, Option[AttackChoice])

  private val data = new ObservableBuffer[AttackPair]()
  data ++= attacks(attacker, defender, attackerHex, defenderHex)

  private val table = new TableView[AttackPair](data) {
    tableColumns foreach (c => columns += c)
    selectionModel.value.cellSelectionEnabled = false
    selectionModel.value.select(0)
  }

  val okButton = new Button {
    text = Localization("common.ok")
    onAction = { e: ActionEvent =>
      val index = table.selectionModel.value.selectedIndex.value
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
    content = new VBox {
      content = List(table, new HBox() {
        content = List(okButton, cancelButton)
        alignment = Pos.CENTER_RIGHT
      })
    }
  }

  private def tableColumns: List[TableColumn[AttackPair, _]] = {
    val attackersAttackColumn = new TableColumn[AttackPair, String] {
      text = Localization("attack.attack")
      cellValueFactory = { c => StringProperty(c.value._1.image) }
    }

    val attackersDamageColumn = new TableColumn[AttackPair, String] {
      text = Localization("attack.damage")
      cellValueFactory = { c =>
        val str = c.value._1.damage + " * " + c.value._1.count
        StringProperty(str)
      }
    }

    val attackersChanceColumn = new TableColumn[AttackPair, String] {
      text = Localization("attack.chance")
      cellValueFactory = { c =>
        val str = c.value._1.chance + "%"
        StringProperty(str)
      }
    }

    val defendersAttackColumn = new TableColumn[AttackPair, String] {
      text = Localization("attack.attack")
      cellValueFactory = { c => StringProperty(c.value._2.map(_.image).getOrElse("")) }
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
    }

    val defendersChanceColumn = new TableColumn[AttackPair, String] {
      text = Localization("attack.chance")
      cellValueFactory = { c =>
        val str = c.value._2.map(_.chance).getOrElse("0") + "%"
        StringProperty(str)
      }
    }

    List(attackersAttackColumn, attackersDamageColumn, attackersChanceColumn,
      defendersAttackColumn, defendersDamageColumn, defendersChanceColumn)
  }

  private def attacks(attacker: Soldier, defender: Soldier, attackerHex: TerrainHex,
    defenderHex: TerrainHex): List[AttackPair] = {
    val attacks = attacker.soldierType.attacks
    attacks map (a => {
      val defendersAttackOpt = Attack.selectBestAttackForDefender(attacker, defender, a)
      val attackDamage = Attack.possibleAttackersDamage(true, attacker, defender, a, defendersAttackOpt)
      val attackesChance = a.chanceOfSuccess(defender.soldierType.defence(defenderHex.terrain))

      val attackerChoice = AttackChoice(a.imageName, attackDamage, a.count, attackesChance)
      defendersAttackOpt match {
        case Some(defendersAttack) => {
          val defenderDamage = Attack.possibleAttackersDamage(false, defender, attacker, defendersAttack, Some(a))
          val defenderChance = defendersAttack.chanceOfSuccess(attacker.soldierType.defence(attackerHex.terrain))
          val defenderChoice = AttackChoice(defendersAttack.imageName, defenderDamage, defendersAttack.count, defenderChance)
          (attackerChoice, Some(defenderChoice))
        }
        case None => {
          (attackerChoice, None)
        }
      }
    })
  }
}