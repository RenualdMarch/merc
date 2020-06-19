package mr.merc.ui.world

import mr.merc.economics.BusinessProject
import mr.merc.local.Localization
import mr.merc.politics.Province
import org.tbee.javafx.scene.layout.MigPane
import scalafx.scene.control.ScrollPane
import scalafx.scene.layout.Region
import scalafx.Includes._

class BusinessProjectsReportPane(map:Map[Province, List[BusinessProject]]) extends ScrollPane {

  fitToWidth = true

  content = new MigPane {

    style = "-fx-background-color: white;"

    map.foreach { case (p, list) =>
      val (completed, inProgress) = list.partition(_.isComplete)
      add(BigText(p.name), "wrap, center, grow, push, span")
      add(BigText(Localization("projectsReport.completedProjects")), "wrap, center, grow, push, span")
      add(buildTable(completed), "wrap, grow, span")
      add(BigText(Localization("projectsReport.inProgress")), "wrap, center, grow, push, span")
      add(buildTable(inProgress), "wrap, grow, span")
    }
  }

  def buildTable(projects:List[BusinessProject]):Region = {
    val captions = List("project", "project.remainingMoney", "project.remainingResources", "project.alreadyBoughtProducts").map(s => Localization(s))
    val rows = projects.flatMap(projectRow)
    GridPaneBuilder.buildWithCaptionString(List(30, 10, 30, 30), rows, captions)
  }

  def projectRow(project: BusinessProject): List[String] = {
    import EconomicLocalization._
    List(
      localizeProject(project),
      DoubleFormatter().format(project.remainingMoney),
      localizeProductsBucket(project.remainingProducts),
      localizeProductsBucket(project.alreadyBoughtProducts)
    )
  }
}
