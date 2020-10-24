package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.Resolver
import com.github.nhweston.dfydoc.node.decl.Module

import scala.xml.{Node, Text}

case class Import(
  name: String,
  target: Token,
  opened: Boolean,
  token: Token,
) extends Decl {

  override def toHtml(implicit ctx: Resolver): Node = Text("")

  def module(implicit ctx: Resolver): Module =
    target.decl.asInstanceOf[Module]

}
