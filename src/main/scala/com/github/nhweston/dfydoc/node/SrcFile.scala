package com.github.nhweston.dfydoc.node

import java.io.{BufferedOutputStream, File, FileOutputStream}

import com.github.nhweston.dfydoc.Resolver
import play.api.libs.json.Json

import scala.xml.Node

case class SrcFile(
  path: String,
  decls: Seq[Decl],
  includes: Seq[String],
) extends Src {

  lazy val name = path.split('/').last

  def write(root: String)(implicit ctx: Resolver): Unit = {
    val f = new File(root, s"$name.html")
    val bos = new BufferedOutputStream(new FileOutputStream(f))
    bos.write("<!DOCTYPE html>".getBytes)
    bos.write("<head><link rel=\"stylesheet\" href=\"styles.css\"></head>\n".getBytes)
    bos.write(html.toString.getBytes)
    bos.close()
  }

  def html(implicit ctx: Resolver): Node =
    <html>
      <head>
        <title>{name}</title>
        <head><link rel="stylesheet" href="styles.css"/></head>
      </head>
      <body>
        <div id="content">
          <h1>{name}</h1>
          {decls.map(_.toHtml)}
        </div>
      </body>
    </html>

}

object SrcFile {

  implicit lazy val fmt = Json.format[SrcFile]

}
