package com.github.nhweston.dfydoc.node

import java.io.{BufferedOutputStream, File, FileOutputStream}

import scala.xml.{Node, Text}

case class SrcDir(
  name: String,
  isRoot: Boolean,
  sub: Map[String, SrcPath],
) extends SrcPath {

  lazy val (files, subdirs) =
    sub.values.toSeq.foldLeft(Seq.empty[SrcFile], Seq.empty[SrcDir]) {
      case ((fs, ds), f: SrcFile) => (fs :+ f, ds)
      case ((fs, ds), d: SrcDir) => (fs, ds :+ d)
    }

  ///////////
  // INDEX //
  ///////////

  lazy val _subdirs =
    subdirs match {
      case Nil => Text("")
      case subdirs =>
        <h2>{if (isRoot) "Directories" else "Subdirectories"}</h2>
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

  def write(root: String): Unit = {
    println(s"Root: $root")
    println(s"Name: $name")
    val fOut = if (isRoot) new File(root) else new File(root, name)
    fOut.mkdir()
    writeIndex(fOut.getAbsolutePath)
    for (file <- files)
      file.write(fOut.getAbsolutePath)
    for (subdir <- subdirs)
      subdir.write(fOut.getAbsolutePath)
  }

  def writeIndex(root: String): Unit = {
    val f = new File(root, "index.html")
    val bos = new BufferedOutputStream(new FileOutputStream(f))
    bos.write("<!DOCTYPE html>\n".getBytes)
    bos.write(index.toString.getBytes)
    bos.close()
  }

}
