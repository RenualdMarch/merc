package mr.merc.economics


import com.typesafe.config.{Config, ConfigFactory}

trait EconomicConfig {
  val config: Config = ConfigFactory.load("conf/economics")
}
