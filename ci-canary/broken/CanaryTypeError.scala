package canary

/** A compile error by design, compiled only when `just ci-canary` adds this directory. */
object CanaryTypeError {
    val n: Int = "not an Int"
}
