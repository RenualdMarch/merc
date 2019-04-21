package mr.merc.ui.world

import mr.merc.economics._
import mr.merc.economics.SpendingPolicy._
import mr.merc.economics.TaxPolicy._
import mr.merc.local.Localization
import mr.merc.politics.State
import org.tbee.javafx.scene.layout.MigPane
import scalafx.beans.binding.Bindings
import scalafx.beans.property.DoubleProperty
import scalafx.scene.control.{ScrollPane, Separator, Slider}
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.util.StringConverter
import scalafx.Includes._
import scalafx.geometry.{Orientation, Pos}

class BudgetPane(worldState: WorldStateBudgetActions) extends TopTitledBorderPane with WorldInterfaceNode {
  private val state = worldState.playerState

  top = BigText(Localization("budget.title", state.name))
  private val income = new IncomePane(worldState)
  private val spending = new SpendingPane(worldState)
  center = PaneWithTwoHorizontalChildren(income, spending)
  bottom = new BudgetBottomProjectionPane(state.budget.moneyReserve, income.projectedIncome, spending.projectedSpending)
}

class SpendingPane(worldState: WorldStateBudgetActions) extends TopTitledBorderPane with WorldInterfaceNode {
  top = BigText(Localization("budget.spending"))
  private val config = new SpendingConfiguration(worldState)
  center = PaneWithTwoVerticalChildren(config, new SpendingReportPane(worldState.playerState.budget))

  def projectedSpending: DoubleProperty = config.projectedExpenses
}

class SpendingReportPane(budget: StateBudget) extends TopTitledBorderPane with WorldInterfaceNode {
  top = MediumText(Localization("budget.spendingReport"))
  center = {
    def spending(report: Spending): String = {
      val s = budget.history.lastOption.flatMap(_.expenses.get(report)).getOrElse(0)
      IntFormatter().format(s)
    }

    val pane = new MigPane("center")
    pane.add(MediumText(Localization("budget.spending.scholars")))
    pane.add(MediumText(spending(ScholarsSalary)), "wrap")
    pane.add(MediumText(Localization("budget.spending.bureaucrats")))
    pane.add(MediumText(spending(BureaucratsSalary)), "wrap")
    pane.add(MediumText(Localization("budget.spending.pensions")))
    pane.add(MediumText(spending(Pensions)), "wrap")
    pane.add(MediumText(Localization("budget.spending.construction")))
    pane.add(MediumText(spending(Construction)), "wrap")
    pane.add(MediumText(Localization("budget.spending.army")))
    pane.add(MediumText(spending(Army)), "wrap")
    val separator = new Separator {
      style = s"-fx-border-width: ${Components.mediumFontSize / 10} 0 0 0; -fx-border-style: solid;"
      orientation = Orientation.Horizontal
    }
    pane.add(separator, "span 2, growx, wrap")
    pane.add(MediumText(Localization("budget.spending.total")))
    pane.add(MediumText(DoubleFormatter().format(budget.history.last.expenses.values.sum)))
    pane
  }
}

class SpendingConfiguration(worldState: WorldStateBudgetActions) extends TopTitledBorderPane with WorldInterfaceNode {
  top = MediumText(Localization("budget.spendingConfig"))
  val conf = worldState.playerState.budget.spendingPolicyConfig

  private val state = worldState.playerState
  private val regions = worldState.playerRegions

  val scholarsSlider = new PopulationSpendingSlider(Localization("budget.spending.scholars"),
    conf.scholarsNeeds, state.budget.scholarsSpendingFunction(regions))
  scholarsSlider.sliderValue.onChange {
    val budget = state.budget
    worldState.setStateSpending(state, budget.spendingPolicyConfig.copy(scholarsNeeds = scholarsSlider.sliderValue.value))
  }

  val bureaucratsSlider = new PopulationSpendingSlider(Localization("budget.spending.bureaucrats"),
    conf.bureaucratsNeeds, state.budget.bureaucratsSpendingFunction(regions))
  bureaucratsSlider.sliderValue.onChange {
    val budget = state.budget
    worldState.setStateSpending(state, budget.spendingPolicyConfig.copy(bureaucratsNeeds = bureaucratsSlider.sliderValue.value))
  }

  val pensionsSlider = new PopulationSpendingSlider(Localization("budget.spending.pensions"),
    conf.pensionsNeeds, state.budget.pensionsSpendingFunction(regions))
  pensionsSlider.sliderValue.onChange {
    val budget = state.budget
    worldState.setStateSpending(state,budget.spendingPolicyConfig.copy(pensionsNeeds = pensionsSlider.sliderValue.value))
  }

  val centerPane = new MigPane("", "10%[]10%")
  centerPane.add(scholarsSlider, "push, grow, wrap")
  centerPane.add(bureaucratsSlider, "push, grow, wrap")
  centerPane.add(pensionsSlider, "push, grow, wrap")

  center = centerPane

  val projectedExpenses: DoubleProperty = new DoubleProperty()
  projectedExpenses <== scholarsSlider.spendingValue + bureaucratsSlider.spendingValue + pensionsSlider.spendingValue

}

class IncomePane(worldState: WorldStateBudgetActions) extends TopTitledBorderPane with WorldInterfaceNode {
  top = BigText(Localization("budget.income"))
  private val conf = new TaxesConfigurationPane(worldState)
  center = PaneWithTwoVerticalChildren(conf, new IncomeReportPane(worldState.playerState.budget))

  def projectedIncome: DoubleProperty = conf.projectedIncome
}

class IncomeReportPane(budget: StateBudget) extends TopTitledBorderPane with WorldInterfaceNode {
  top = MediumText(Localization("budget.incomeReport"))
  center = {
    def income(report: Income): String = {
      val s = budget.history.lastOption.flatMap(_.income.get(report)).getOrElse(0)
      IntFormatter().format(s)
    }

    val pane = new MigPane("center")
    pane.add(MediumText(Localization("budget.income.corporate")))
    pane.add(MediumText(income(CorporateTax)), "wrap")
    pane.add(MediumText(Localization("budget.income.sales")))
    pane.add(MediumText(income(SalesTax)), "wrap")
    pane.add(MediumText(Localization("budget.income.salary.low")))
    pane.add(MediumText(income(LowSalaryTax)), "wrap")
    pane.add(MediumText(Localization("budget.income.salary.middle")))
    pane.add(MediumText(income(MiddleSalaryTax)), "wrap")
    pane.add(MediumText(Localization("budget.income.salary.upper")))
    pane.add(MediumText(income(UpperSalaryTax)), "wrap")
    pane.add(MediumText(Localization("budget.income.tariff")))
    pane.add(MediumText(income(TariffTax)), "wrap")
    pane.add(MediumText(Localization("budget.income.transit")))
    pane.add(MediumText(income(TransitTax)), "wrap")
    val separator = new Separator {
      style = s"-fx-border-width: ${Components.mediumFontSize / 10} 0 0 0; -fx-border-style: solid;"
      orientation = Orientation.Horizontal
    }
    pane.add(separator, "span 2,growx,wrap")
    pane.add(MediumText(Localization("budget.income.total")))
    pane.add(MediumText(DoubleFormatter().format(budget.history.last.income.values.sum)))
    pane
  }
}

class TaxesConfigurationPane(worldState: WorldStateBudgetActions) extends TopTitledBorderPane with WorldInterfaceNode {
  top = MediumText(Localization("budget.incomeConfig"))

  private val state = worldState.playerState

  private val budget = state.budget

  val salaryLowTax = new TaxLevelSlider(Localization("budget.income.salary.low"),
    budget.taxPolicy(LowSalaryTax), state.rulingParty.economy.salaryTax.min,
    state.rulingParty.economy.salaryTax.max, budget.projectIncomeFunction(LowSalaryTax))
  salaryLowTax.sliderValue.onChange {
    worldState.setStateTax(state, LowSalaryTax, salaryLowTax.sliderValue.value)
  }

  val salaryMiddleTax = new TaxLevelSlider(Localization("budget.income.salary.middle"),
    state.budget.taxPolicy(MiddleSalaryTax),state.rulingParty.economy.salaryTax.min,
    state.rulingParty.economy.salaryTax.max, budget.projectIncomeFunction(MiddleSalaryTax))
  salaryMiddleTax.sliderValue.onChange {
    worldState.setStateTax(state, MiddleSalaryTax, salaryMiddleTax.sliderValue.value)
  }

  val salaryUpperTax = new TaxLevelSlider(Localization("budget.income.salary.upper"),
    state.budget.taxPolicy(UpperSalaryTax), state.rulingParty.economy.salaryTax.min,
    state.rulingParty.economy.salaryTax.max, budget.projectIncomeFunction(UpperSalaryTax))
  salaryUpperTax.sliderValue.onChange {
    worldState.setStateTax(state, UpperSalaryTax, salaryUpperTax.sliderValue.value)
  }

  val corporateTax = new TaxLevelSlider(Localization("budget.income.corporate"),
    state.budget.taxPolicy(CorporateTax), state.rulingParty.economy.corporateTax.min,
    state.rulingParty.economy.corporateTax.max, budget.projectIncomeFunction(CorporateTax))
  corporateTax.sliderValue.onChange {
    worldState.setStateTax(state, CorporateTax, corporateTax.sliderValue.value)
  }

  val salesTax = new TaxLevelSlider(Localization("budget.income.sales"),
    state.budget.taxPolicy(SalesTax), state.rulingParty.economy.salesTax.min,
    state.rulingParty.economy.salesTax.max, budget.projectIncomeFunction(SalesTax))
  salesTax.sliderValue.onChange {
    worldState.setStateTax(state, SalesTax, salesTax.sliderValue.value)
  }


  val tariffTax = new TaxLevelSlider(Localization("budget.income.tariff"),
    state.budget.taxPolicy(TariffTax), state.rulingParty.economy.tariff.min,
    state.rulingParty.economy.tariff.max, budget.projectIncomeFunction(TariffTax))
  tariffTax.sliderValue.onChange {
    worldState.setStateTax(state, TariffTax, tariffTax.sliderValue.value)
  }

  val transitTax = new TaxLevelSlider(Localization("budget.income.transit"),
    state.budget.taxPolicy(TransitTax), state.rulingParty.economy.transit.min,
    state.rulingParty.economy.transit.max, budget.projectIncomeFunction(TransitTax))
  transitTax.sliderValue.onChange {
    worldState.setStateTax(state, TransitTax, transitTax.sliderValue.value)
  }

  val centerPane = new MigPane("", "10%[][10%][50%][20%]5%") with WorldInterfaceWhiteJavaNode

  def addPane(taxLevelSlider: TaxLevelSlider): Unit = {
    centerPane.add(taxLevelSlider.titleLabel)
    centerPane.add(taxLevelSlider.money)
    centerPane.add(taxLevelSlider.slider, "grow")

    val s = Bindings.createStringBinding(() => DoubleFormatter().format(taxLevelSlider.projectedIncome.value), taxLevelSlider.projectedIncome)

    centerPane.add(MediumText(s), "wrap")
  }

  addPane(salaryLowTax)
  addPane(salaryMiddleTax)
  addPane(salaryUpperTax)
  addPane(corporateTax)
  addPane(salesTax)
  addPane(tariffTax)
  addPane(transitTax)

  center = centerPane

  val projectedIncome: DoubleProperty = new DoubleProperty()
  projectedIncome <== salaryLowTax.projectedIncome + salaryMiddleTax.projectedIncome + salaryUpperTax.projectedIncome +
    corporateTax.projectedIncome + salesTax.projectedIncome + tariffTax.projectedIncome + transitTax.projectedIncome
}

class BudgetBottomProjectionPane(reserves: Double, income: DoubleProperty, expenses: DoubleProperty) extends TopTitledBorderPane with WorldInterfaceNode {
  center = {
    val pane = new MigPane()
    pane.add(MediumText(Localization("budget.projected.gain")))
    val projectedIncomeText = new MediumText()
    projectedIncomeText.text <== Bindings.createStringBinding(() => DoubleFormatter().format(income.value), income)
    pane.add(projectedIncomeText)
    pane.add(MediumText("-"))
    val projectedExpensesText = new MediumText()
    projectedExpensesText.text <== Bindings.createStringBinding(() => DoubleFormatter().format(expenses.value), expenses)
    pane.add(projectedExpensesText)
    pane.add(MediumText("="))
    val projectedGainText = new MediumText()
    projectedGainText.text <== Bindings.createStringBinding(() => DoubleFormatter().format(income.value - expenses.value), income, expenses)
    pane.add(projectedGainText)
  }
  left = {
    val pane = new MigPane()
    pane.add(MediumText(Localization("budget.reserve")))
    val reserveText = new MediumText()
    reserveText.text = DoubleFormatter().format(reserves)
    pane.add(reserveText)
  }

  BorderPane.setAlignment(center.value, Pos.Center)
}


class TaxLevelSlider(title: String, initialValue: Double, minValue: Double, maxValue: Double, projectedValue: Double => Double) {
  val slider = new Slider(0, 1d, initialValue) {
    style = Components.mediumFontStyle
    value.onChange {
      if (this.value.value > maxValue) {
        this.value = maxValue
      } else if (this.value.value < minValue) {
        this.value = minValue
      }
    }
    labelFormatter = new StringConverter[Double] {
      override def fromString(string: String): Double =
        if (string == Localization("budget.min")) minValue
        else if (string == Localization("budget.max")) maxValue
        else maxValue

      override def toString(t: Double): String =
        if (t <= minValue) Localization("budget.min")
        else if (t <= maxValue) Localization("budget.max")
        else Localization("budget.max")
    }
  }

  def sliderValue: DoubleProperty = slider.value

  val money = new MediumText()
  money.text <== Bindings.createStringBinding(() => IntFormatter().format(slider.value.value * 100) + "%", slider.value)

  val titleLabel = BigText(title)

  val projectedIncome: DoubleProperty = new DoubleProperty()
  projectedIncome <== Bindings.createDoubleBinding(() => projectedValue(sliderValue.value), sliderValue)

}

class PopulationSpendingSlider(title: String, initialPositionPerc: Double, percToSpending: Double => Double) extends TopTitledBorderPane {
  private val slider = new Slider(0d, 1d, initialPositionPerc) {
    style = Components.mediumFontStyle
    showTickLabels = true
    showTickMarks = true
    majorTickUnit = 1d / 3
    //minorTickCount = 3
    private val minValue = 0d
    private val maxValue = 1d
    value.onChange {
      if (this.value.value > maxValue) {
        this.value = maxValue
      } else if (this.value.value < minValue) {
        this.value = minValue
      }
    }

    snapToTicks = false
    labelFormatter = new StringConverter[Double] {
      override def fromString(string: String): Double =
        if (string == Localization("budget.noNeeds")) 0d
        else if (string == Localization("budget.lifeNeeds")) 1d / 3
        else if (string == Localization("budget.regularNeeds")) 2d / 3
        else 1d

      override def toString(t: Double): String =
        if (t <= 0d) Localization("budget.noNeeds")
        else if (t <= 1d / 3) Localization("budget.lifeNeeds")
        else if (t <= 2d / 3) Localization("budget.regularNeeds")
        else Localization("budget.luxuryNeeds")
    }
  }

  def sliderValue: DoubleProperty = slider.value

  val spendingValue: DoubleProperty = new DoubleProperty()
  spendingValue <== Bindings.createDoubleBinding(() => percToSpending(sliderValue.value), sliderValue)
  private val titleWithMoney = BigText("")
  titleWithMoney.text <== Bindings.createStringBinding(() => title + " " + DoubleFormatter().format(percToSpending(slider.value.value)), slider.value)

  top = titleWithMoney
  center = slider

}