package hydrozoa.integration

import ch.qos.logback.classic.PatternLayout

class MermaidSequenceDiagramLayout extends PatternLayout {
    override def getFileHeader: String = "sequenceDiagram"
}
