package com.jichao.errors

final case class InvalidEnumValue(private val message: String = "",
                                  private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

final case class NextException(private val message: String = "",
                               private val cause: Throwable = None.orNull)
  extends Exception(message, cause)