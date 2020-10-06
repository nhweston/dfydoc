package com.github.nhweston.dfydoc

import com.github.nhweston.dfydoc.Resolver._
import com.github.nhweston.dfydoc.node._

case class Resolver(root: SrcDir) {

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
