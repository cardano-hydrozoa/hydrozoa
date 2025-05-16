package hydrozoa.sut

import com.bloxbean.cardano.client.api.model.ProtocolParams
import com.fasterxml.jackson.databind.ObjectMapper

import scala.io.Source

object Utils:
    def protocolParams: ProtocolParams =
        new ObjectMapper()
            .reader()
            .readValue(
              Source.fromResource("protocolParams.json").bufferedReader(),
              classOf[ProtocolParams]
            )
