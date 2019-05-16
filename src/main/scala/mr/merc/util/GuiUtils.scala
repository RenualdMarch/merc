package mr.merc.util

import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.collections.ObservableList
import javafx.scene.control.{TableColumn, TableColumnBase}
import javafx.scene.control.skin.TableViewSkinBase
import scalafx.scene.control.TableView

object GuiUtils {
  private val columnToFitMethod = {
    val cls = Class.forName("javafx.scene.control.skin.TableSkinUtils")
    val m = cls.getDeclaredMethod("resizeColumnToFitContent", classOf[TableViewSkinBase[_, _, _, _, _]], classOf[TableColumnBase[_, _]], Integer.TYPE)
    m.setAccessible(true)
    m
  }

  def autoFitTable[T](table:TableView[T]): Unit = {
    table.items.addListener(new ChangeListener[ObservableList[T]] {
      override def changed(observableValue: ObservableValue[_ <: ObservableList[T]],
                           t: ObservableList[T], t1: ObservableList[T]): Unit = {
        table.columns.foreach {c =>
          columnToFitMethod.invoke(table.delegate.getSkin, c, int2Integer(-1))
        }
      }
    })
  }
}
