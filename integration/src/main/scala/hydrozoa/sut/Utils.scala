package hydrozoa.sut

import com.bloxbean.cardano.client.api.model.ProtocolParams
import com.fasterxml.jackson.databind.ObjectMapper
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import com.typesafe.scalalogging.Logger
import sttp.client4.quick.*
import sttp.client4.{Response, ResponseAs}

import scala.io.Source

object Utils:
    def protocolParams: ProtocolParams =
        new ObjectMapper()
            .reader()
            .readValue(
              Source.fromResource("protocolParams.json").bufferedReader(),
              classOf[ProtocolParams]
            )

