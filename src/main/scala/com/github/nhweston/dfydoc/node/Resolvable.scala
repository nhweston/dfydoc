package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.Resolver

trait Resolvable {

  def name: String
  def token: Token
  def children: Seq[Resolvable] = Seq.empty

  def aname(implicit ctx: Resolver): String =
    ctx.tokensToPaths(token).anchor.mkString("~")

}
