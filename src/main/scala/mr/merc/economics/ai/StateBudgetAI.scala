package mr.merc.economics.ai

import mr.merc.economics.StateBudget
import mr.merc.politics.IssuePosition.EconomyPosition
import mr.merc.politics.State

object StateBudgetAI {
  def apply(): StateBudgetAI = new SocialDemocraticStateBudgetAI()
}

trait StateBudgetAI {
  def balanceBudget(state: State): Unit
}

class SocialDemocraticStateBudgetAI extends StateBudgetAI {
  private val idealMoneyReserve = 25000d
  private val step = 0.2

  def balanceBudget(state: State): Unit = {
    implicit val budget:StateBudget = state.budget
    implicit val position = state.politicalSystem.rulingParty.economy
    if (tooMuchMoney) {
      if (canIncreaseSpendingToBureaucracyAndScholars) {
        increaseSpendingToBureaucracyAndScholars()
      } else if (canIncreaseSocialSpending){
        increaseSocialSpending()
      } else if (canDecreaseTaxes) {
        decreaseTaxes()
      }
    } else {
      if (canIncreaseTaxes) {
        increaseTaxes()
      } else if (canDecreaseSocialSpending) {
        decreaseSocialSpending()
      } else if (canDecreaseSpendingToBureaucracyAndScholars) {
        decreaseSpendingToBureaucracyAndScholars()
      }
    }
  }

  private def tooMuchMoney(implicit budget:StateBudget): Boolean = budget.moneyReserve > idealMoneyReserve

  private def canIncreaseSpendingToBureaucracyAndScholars(implicit budget:StateBudget):Boolean = {
    math.min(budget.spendingPolicyConfig.bureaucratsNeeds, budget.spendingPolicyConfig.scholarsNeeds) < 1.0
  }

  private def canDecreaseSpendingToBureaucracyAndScholars(implicit budget:StateBudget):Boolean = {
    math.max(budget.spendingPolicyConfig.bureaucratsNeeds, budget.spendingPolicyConfig.scholarsNeeds) > 0d
  }

  private def increaseSpendingToBureaucracyAndScholars()(implicit budget:StateBudget): Unit = {
    val max = Math.min(budget.spendingPolicyConfig.scholarsNeeds, budget.spendingPolicyConfig.bureaucratsNeeds)
    val next = Math.min(1d, max + step)
    budget.spendingPolicyConfig = budget.spendingPolicyConfig.copy(scholarsNeeds = next, bureaucratsNeeds = next)
  }

  private def decreaseSpendingToBureaucracyAndScholars()(implicit budget:StateBudget): Unit = {
    val min = Math.max(budget.spendingPolicyConfig.scholarsNeeds, budget.spendingPolicyConfig.bureaucratsNeeds)
    val next = Math.max(0d, min - step)
    budget.spendingPolicyConfig = budget.spendingPolicyConfig.copy(scholarsNeeds = next, bureaucratsNeeds = next)
  }

  private def canIncreaseSocialSpending()(implicit budget:StateBudget): Boolean = {
    budget.spendingPolicyConfig.pensionsNeeds < 1d
  }

  private def canDecreaseSocialSpending()(implicit budget:StateBudget): Boolean = {
    budget.spendingPolicyConfig.pensionsNeeds > 0d
  }

  private def increaseSocialSpending()(implicit budget:StateBudget): Unit = {
    val conf = budget.spendingPolicyConfig
    val increase = Math.min(1d, conf.pensionsNeeds + step)
    budget.spendingPolicyConfig = conf.copy(pensionsNeeds = increase)
  }

  private def decreaseSocialSpending()(implicit budget:StateBudget): Unit = {
    val conf = budget.spendingPolicyConfig
    val increase = Math.max(0d, conf.pensionsNeeds - step)
    budget.spendingPolicyConfig = conf.copy(pensionsNeeds = increase)
  }

  private def canIncreaseTaxes()(implicit budget:StateBudget, economyPosition: EconomyPosition): Boolean = {
    val maxTaxes = economyPosition.maxTaxPolicy
    budget.taxPolicy.taxPolicyValues.exists { case (k, v) =>
      v < maxTaxes(k)
    }
  }

  private def canDecreaseTaxes()(implicit budget:StateBudget, economyPosition: EconomyPosition): Boolean = {
    val minTaxes = economyPosition.minTaxPolicy
    budget.taxPolicy.taxPolicyValues.exists { case (k, v) =>
      v > minTaxes(k)
    }
  }

  private def increaseTaxes()(implicit budget:StateBudget, economyPosition: EconomyPosition): Unit = {
    val maxTaxes = economyPosition.maxTaxPolicy
    val minTaxes = economyPosition.minTaxPolicy
    budget.taxPolicy.taxPolicyValues.foreach { case (income, value) =>
      val taxStep = (maxTaxes(income) - minTaxes(income)) * step
      val newValue = Math.min(value + taxStep, maxTaxes(income))
      budget.taxPolicy.set(income, newValue)
    }
  }

  private def decreaseTaxes()(implicit budget:StateBudget, economyPosition: EconomyPosition): Unit = {
    val maxTaxes = economyPosition.maxTaxPolicy
    val minTaxes = economyPosition.minTaxPolicy
    budget.taxPolicy.taxPolicyValues.foreach { case (income, value) =>
      val taxStep = (maxTaxes(income) - minTaxes(income)) * step
      val newValue = Math.max(value - taxStep, minTaxes(income))
      budget.taxPolicy.set(income, newValue)
    }
  }
}
