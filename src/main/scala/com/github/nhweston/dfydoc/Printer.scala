package com.github.nhweston.dfydoc

import java.io.File
import java.nio.file.Paths

case class Printer(
  f: File,
  asJson: Boolean,
)(implicit sr: ServerRunner) {

  def print(): Unit =
    if (asJson)
      println(Util.pprintJson(sr.getJson(f.getAbsolutePath)))
    else {
      val tree = sr.getTree(f.getAbsolutePath)
      pprint.pprintln(tree)
      val path = Paths.get(f.getAbsolutePath).toRealPath().toFile.getParent
      val dir = PathResolver(path, tree).result
      pprint.pprintln(dir)
    }

}
