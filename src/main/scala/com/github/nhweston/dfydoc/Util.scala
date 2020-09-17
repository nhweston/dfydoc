package com.github.nhweston.dfydoc

import play.api.libs.json._

object Util {

  // The Play JSON pretty printer is awful.
  def pprintJson(json: JsValue, tab: Int = 0): String =
    json match {
      case JsNull => "null"
      case JsTrue => "true"
      case JsFalse => "false"
      case JsNumber(num) => num.toString
      case JsString(str) =>
        val esc = str
          .replaceAllLiterally("\\", "\\\\")
          .replaceAllLiterally("\"", "\\\"")
          .replaceAllLiterally("\n", "\\n")
        s""""$esc""""
      case JsArray(arr) =>
        if (arr.isEmpty) "[]"
        else arr
          .map(v => " " * (tab + 2) + pprintJson(v, tab + 2))
          .mkString("[\n", ",\n", "\n" + " " * tab + "]")
      case JsObject(map) =>
        if (map.isEmpty) "{}"
        else map
          .map[String] { case (k, v) => " " * (tab + 2) + s""""$k": """ + pprintJson(v, tab + 2) }
          .mkString("{\n", ",\n", "\n" + " " * tab + "}")
    }

}
