package mr.merc.log

import org.slf4j.LoggerFactory

trait Logging {
  private val logger = LoggerFactory.getLogger(getClass())

  def debug(msg: => String) {
    if (logger.isDebugEnabled()) {
      logger.debug(msg)
    }
  }

  def debug(msg: => String, er: Throwable) {
    if (logger.isDebugEnabled()) {
      logger.debug(msg, er)
    }
  }

  def info(msg: => String) {
    if (logger.isInfoEnabled()) {
      logger.info(msg)
    }
  }

  def info(msg: => String, er: Throwable) {
    if (logger.isInfoEnabled()) {
      logger.info(msg, er)
    }
  }

  def warn(msg: => String) {
    if (logger.isWarnEnabled()) {
      logger.warn(msg)
    }
  }

  def warn(msg: => String, er: Throwable) {
    if (logger.isWarnEnabled()) {
      logger.warn(msg, er)
    }
  }

  def error(msg: => String) {
    if (logger.isErrorEnabled()) {
      logger.error(msg)
    }
  }

  def error(msg: => String, er: Throwable) {
    if (logger.isErrorEnabled()) {
      logger.error(msg, er)
    }
  }
}