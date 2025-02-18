package hydrozoa.logging

trait LoggingService {
  def logInfo(message: String): Unit

  def logError(message: String): Unit
}