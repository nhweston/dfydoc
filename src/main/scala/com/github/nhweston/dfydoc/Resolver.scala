package com.github.nhweston.dfydoc

import com.github.nhweston.dfydoc.Resolver._
import com.github.nhweston.dfydoc.node._

import scala.annotation.tailrec

case class Resolver(root: SrcPath) {

  lazy val (tokensToPaths, tokensToNodes): (TokensToPaths, TokensToNodes) = {
    type Result = (TokensToPaths, TokensToNodes)
    val bttp = Map.newBuilder[Token, DeclPath]
    val bttn = Map.newBuilder[Token, Resolvable]

    def aux0(sp: SrcPath, path: DeclPath): Unit =
      sp match {
        case SrcDir(name, isRoot, sub) =>
          val f =
            if (isRoot) aux0(_, DeclPath())
            else aux0(_, DeclPath(Seq(name)))
          for ((_, sp) <- sub) f(sp)
        case sf @ SrcFile(_, decls) =>
          for (decl <- decls)
            aux1(decl, path /+ sf.name)
      }

    def aux1(decl: Resolvable, path: DeclPath): Unit = {
      bttp += (decl.token -> (path #+ decl.name))
      bttn += (decl.token -> decl)
      for (child <- decl.children)
        aux1(child, path #+ decl.name)
    }

    aux0(root, DeclPath())
    (bttp.result(), bttn.result())
  }

  def getRelativePath(target: Resolvable, root: Resolvable): String =
    getRelativePath(target.token, root.token)

  def getRelativePath(target: Token, root: Token): String =
    (tokensToPaths.get(target), tokensToPaths.get(root)) match {
      case (Some(tp), Some(rp)) =>
        val tDirs :+ _ = tp.file
        val rDirs :+ rName = rp.file
        @tailrec
        def aux(
          tDirs: Seq[String] = tDirs,
          rDirs: Seq[String] = rDirs,
        ): Seq[String] =
          (tDirs, rDirs) match {
            case (thd +: ttl, rhd +: rtl) if (thd == rhd) =>
              // pop common ancestor
              aux(ttl, rtl)
            case _ =>
              // move up to inner-most common ancestor and append remaining path
              rDirs.map(_ => "..") ++ tDirs
          }
        aux().mkString("/") + rName + ".html#" + tp.anchor.mkString("~")
      case _ => ""
    }

  def getAnchorName(token: Token): String =
    tokensToPaths.get(token) match {
      case Some(path) => path.anchor.mkString("~")
      case None => ""
    }

}

object Resolver {

  type TokensToPaths = Map[Token, DeclPath]
  type TokensToNodes = Map[Token, Resolvable]

  case class DeclPath(
    file: Seq[String] = Seq.empty,
    anchor: Seq[String] = Seq.empty,
  ) {

    def appendFilePath(name: String) =
      copy(file = file :+ name)

    def appendAnchorPath(name: String) =
      copy(anchor = anchor :+ name)

    def /+(name: String) = appendFilePath(name)

    def #+(name: String) = appendAnchorPath(name)

  }

}
