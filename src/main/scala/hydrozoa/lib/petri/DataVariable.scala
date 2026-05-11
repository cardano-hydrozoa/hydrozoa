package hydrozoa.lib.petri

enum DataVariable[Id](id: Id):
    case Int(int: scala.Int, id: Id) extends DataVariable(id)
    // See : https://github.com/chimenkamp/YAPNE-Yet-Another-Petri-Net-Editor/blob/d0f55c3f2bda900560cf5d30ca535ef990df1124/src/extensions/guard-language/guard-migrator.js#L147
    case Real(real: Double, id: Id) extends DataVariable(id)
    case Boolean(boolean: scala.Boolean, id: Id) extends DataVariable(id)
