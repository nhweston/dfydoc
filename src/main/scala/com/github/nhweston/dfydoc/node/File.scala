package com.github.nhweston.dfydoc.node

import com.github.nhweston.dfydoc.DocNode

import scala.xml.Node

case class File(
  name: String,
  decls: Seq[DocNode],
) extends DocNode {

  override def doc: Option[String] = None

  override lazy val toHtml: Node =
    <html>
      <head>
        <title>{name}</title>
      </head>
      <body>
        <h1>{name}</h1>
        {decls.map(_.toHtml)}
      </body>
    </html>

}
