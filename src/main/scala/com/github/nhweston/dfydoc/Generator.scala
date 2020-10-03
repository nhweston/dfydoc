package com.github.nhweston.dfydoc

import java.io.File

import com.github.nhweston.dfydoc.node.SrcDir

case class Generator(
  fIn: File,
  fOut: File,
)(implicit sr: ServerRunner) {

  lazy val root = {
    if (!fIn.isDirectory)
      throw new Exception(s"${fIn.isDirectory} is not a directory")
    if (fOut.isFile)
      throw new Exception(s"${fOut.getAbsolutePath} is a file")
    if (!fOut.exists)
      fOut.mkdir()
    SrcDir(fIn, fOut, s"${fIn.getName}")
  }

  def write(): Unit = root.write()

}
