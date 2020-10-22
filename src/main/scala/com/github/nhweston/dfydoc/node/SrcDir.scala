package com.github.nhweston.dfydoc.node

import java.io.{BufferedOutputStream, File, FileOutputStream}

import com.github.nhweston.dfydoc.Resolver

import scala.xml.{Node, Text}

/** Represents a source directory. Note that `path` is `None` if this is the root directory. */
case class SrcDir(
  path: Option[String],
  sub: Map[String, Src] = Map.empty,
) extends Src {

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
        <h2>{if (path.isEmpty) "Directories" else "Subdirectories"}</h2>
        <ul>{
          subdirs.map { subdir =>
            <li><a href={s"${subdir.path.get}/index.html"}>{subdir.path.get}</a></li>
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

  def index(implicit ctx: Resolver): Node =
    <html>
      <head>
        <title>{path.getOrElse(ctx.projectName)}</title>
      </head>
      <body>
        <h1>{path.getOrElse(ctx.projectName)}</h1>
        {_subdirs}
        {_files}
      </body>
    </html>

  ///////////
  // WRITE //
  ///////////

  def write(root: String)(implicit ctx: Resolver): Unit = {
    val fOut =
      path match {
        case Some(path) => new File(root, path)
        case None => new File(root)
      }
    fOut.mkdir()
    writeIndex(fOut.getAbsolutePath)
    for (file <- files)
      file.write(fOut.getAbsolutePath)
    for (subdir <- subdirs)
      subdir.write(fOut.getAbsolutePath)
  }

  def writeIndex(root: String)(implicit ctx: Resolver): Unit = {
    val f = new File(root, "index.html")
    val bos = new BufferedOutputStream(new FileOutputStream(f))
    bos.write("<!DOCTYPE html>\n".getBytes)
    bos.write("<head><link rel=\"stylesheet\" href=\"styles.css\"></head>\n".getBytes)
    bos.write(index.toString.getBytes)
    bos.close()
  }

}
