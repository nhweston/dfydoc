package com.github.nhweston.dfydoc

import java.io.File

case class Printer(
  f: File,
  asJson: Boolean,
)(implicit sr: ServerRunner) {

  def print(): Unit =
    if (asJson)
      println(Util.pprintJson(sr.getJson(f.getAbsolutePath)))
    else
      pprint.pprintln(sr.getTree(f.getAbsolutePath))

}
