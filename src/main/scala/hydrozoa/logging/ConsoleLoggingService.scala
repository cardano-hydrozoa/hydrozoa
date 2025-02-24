package hydrozoa.logging

class ConsoleLoggingService extends LoggingService {
    override def logInfo(message: String) = println(s"INFO: $message")

    override def logError(message: String) = println(s"ERROR: $message")
}
