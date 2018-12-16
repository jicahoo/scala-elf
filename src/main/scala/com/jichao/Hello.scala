package com.jichao

import com.typesafe.scalalogging.Logger

class Hello {

}
object Hello {
  val logger = Logger(classOf[Hello])

  def logDemo(): Unit = {
    logger.debug("Hello Log Demo")
  }

}
