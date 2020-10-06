package com.github.nhweston.dfydoc.node

trait Resolvable {

  def name: String
  def token: Token
  def children: Seq[Resolvable] = Seq.empty

}
