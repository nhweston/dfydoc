package com.github.nhweston.dfydoc.node

trait SrcPath extends DocNode {

  def create(
    file: SrcFile,
    path: Seq[String],
  ): SrcPath =
    (this, path) match {
      case (self @ SrcDir(_, _, sub), fileName +: Nil) =>
        self.copy(
          sub = sub.updatedWith(fileName) {
            case Some(existing) => Some(existing)
            case None => Some(file)
          }
        )
      case (self @ SrcDir(_, _, sub), dir +: (tl @ _ +: _)) =>
        self.copy(
          sub = sub.updatedWith(dir) {
            case Some(existing) => Some(existing.create(file, tl))
            case None => Some(SrcDir(dir, false, Map.empty).create(file, tl))
          }
        )
      case _ => this
    }

}
