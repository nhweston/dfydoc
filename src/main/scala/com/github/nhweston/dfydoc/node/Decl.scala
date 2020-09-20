package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.DocNode
import play.api.libs.json.Json

trait Decl extends DocNode

object Decl {

  object Modifier extends Enumeration {
    val `abstract` = Value
    val ghost = Value
    val static = Value
    val `protected` = Value
  }

  type Modifier = Modifier.Value

  implicit lazy val fmtModifier = Json.formatEnum(Modifier)

}
