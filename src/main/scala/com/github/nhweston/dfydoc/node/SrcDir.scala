package com.github.nhweston.dfydoc.node

import java.io.{BufferedOutputStream, File, FileOutputStream}

import com.github.nhweston.dfydoc.ServerRunner

import scala.xml.{Node, Text}

case class SrcDir(
  inParent: File,
  outParent: File,
  name: String,
)(implicit sr: ServerRunner) {

  ///////////
  // FILES //
  ///////////

  lazy val fIn = new File(inParent.getAbsolutePath, name)

  lazy val fOut = new File(outParent.getAbsolutePath, name)

  lazy val sub: Seq[File] = outParent.listFiles.toSeq

  lazy val (fSubdirs, fFiles) = sub.partition(_.isDirectory)

  lazy val subdirs: Seq[SrcDir] =
    fSubdirs.map(s => SrcDir(fIn, fOut, s.getName))

  lazy val files: Seq[SrcFile] =
    fFiles
      .withFilter(f => f.getName.endsWith(".dfy"))
      .map(f => SrcFile(fIn, fOut, f.getName))

  ///////////
  // INDEX //
  ///////////

  lazy val _subdirs =
    subdirs match {
      case Nil => Text("")
      case subdirs =>
        <h2>Subdirectories</h2>
        <ul>{
          subdirs.map { subdir =>
            <li><a href={s"${subdir.name}/index.html"}>{subdir.name}</a></li>
          }
        }</ul>
    }

  lazy val _files =
    files match {
      case Nil => Text("")
      case files =>
        <h2>Files</h2>
        <ul>{
          files.map { file =>
            <li><a href={s"${file.name}.html"}>{file.name}</a></li>
          }
        }</ul>
    }

  lazy val index: Node =
    <html>
      <head>
        <title>{name}</title>
      </head>
      <body>
        <h1>{name}</h1>
        {_subdirs}
        {_files}
      </body>
    </html>

  ///////////
  // WRITE //
  ///////////

  def write(): Unit = {
    fOut.mkdir()
    writeIndex()
    for (file <- files)
      file.write()
    for (subdir <- subdirs)
      subdir.write()
  }

  def writeIndex(): Unit = {
    val f = new File(fOut.getName)
    val bos = new BufferedOutputStream(new FileOutputStream(f))
    bos.write("<!DOCTYPE html>\n".getBytes)
    bos.write(index.toString.getBytes)
    bos.close()
  }

}
