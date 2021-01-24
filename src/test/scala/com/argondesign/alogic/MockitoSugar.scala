////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Syntax sugar for Mockito. Note we are not using
// "scalatest / scalatestplus-mockito "because it uses an old version of
// Mockito and does not have much functionality at all.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import org.mockito.Mockito
import org.mockito.ArgumentMatchers
import org.mockito.stubbing.OngoingStubbing
import org.mockito.verification.VerificationMode

import scala.reflect.ClassTag

object MockitoSugar {

  def mock[T: ClassTag]: T =
    Mockito.mock(implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]])

  // Extension methods
  implicit class Extensions[T <: AnyRef](val subject: T) extends AnyVal {

    def willReturn(result: T): Unit = Mockito.when(subject).thenReturn(result)

    def verify(f: (=> T) => Unit): Unit = {
      f(Mockito.verify(subject))
      Mockito.verifyNoMoreInteractions(subject)
    }

    def verifyIgnoringStubs(f: (=> T) => Unit): Unit = {
      f(Mockito.verify(subject))
      Mockito.verifyNoMoreInteractions(Mockito.ignoreStubs(subject).head)
    }

  }

  // TODO: The following should be just exports in Scala 3
  def when[T](methodCall: T): OngoingStubbing[T] = Mockito.when(methodCall)

  def verify[T <: AnyRef](mock: T): T = Mockito.verify(mock)

  def verify[T <: AnyRef](mock: T, mode: VerificationMode): T = Mockito.verify(mock, mode)

  def verifyNoMoreInteractions(mock: AnyRef): Unit = Mockito.verifyNoMoreInteractions(mock)

  // Almost just export, but with bit of sugar
  def *[T]: T = ArgumentMatchers.any()

}
