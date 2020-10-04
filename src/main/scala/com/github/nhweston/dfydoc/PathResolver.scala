package com.github.nhweston.dfydoc

import java.nio.file.Paths

import com.github.nhweston.dfydoc.node.{SrcDir, SrcFile}

import scala.jdk.javaapi.CollectionConverters._

case class PathResolver(
  root: String,
  files: Seq[SrcFile],
) {

  lazy val rootPath = Paths.get(root).toRealPath()

  lazy val result: SrcDir =
    files.foldLeft(SrcDir(rootPath.getFileName.toString, true, Map.empty)) { case (dir, file) =>
      val path = Paths.get(rootPath.toString, file.path)
      val names = asScala(rootPath.relativize(path).iterator()).map(_.toString).toSeq
      dir.create(file, names).asInstanceOf[SrcDir]
    }

}

object PathResolver {



}
