package com.github.nhweston.dfydoc

import scala.collection.mutable

case class LazyMap[A, B](f: A => B) extends (A => B){

  private val map = mutable.Map.empty[A, B]

  override def apply(a: A): B = map.getOrElseUpdate(a, f(a))

}
