package hydrozoa.lib.petri.net.components

// TODO: YAPNE models "data petri nets" or "DPN". It allows simple typed variables (integer, doubles, and booleans)
// to be checked in transition pre-conditions ("guards") and updated as post-conditions.
// These are restricted to simple expressions, with beta support for arbitrary python.
// One reason I suspect that the language may be so restricted is to make SMT solving easier, but I don't know
// for certain.
type Expression = Unit

enum DataVariable[Id](val id: Id):
    case Int(int: scala.Int, override val id: Id) extends DataVariable(id)
    // See : https://github.com/chimenkamp/YAPNE-Yet-Another-Petri-Net-Editor/blob/d0f55c3f2bda900560cf5d30ca535ef990df1124/src/extensions/guard-language/guard-migrator.js#L147
    case Real(real: Double, override val id: Id) extends DataVariable(id)
    case Boolean(boolean: scala.Boolean, override val id: Id) extends DataVariable(id)

case class DataVariablePresentation(
    name: String,
    description: String
)
