package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.Resolver
import com.github.nhweston.dfydoc.node.decl.Module

case class Import(
  target: Token,
  opened: Boolean,
) {

  def module(implicit ctx: Resolver): Module =
    target.decl.asInstanceOf[Module]

}
