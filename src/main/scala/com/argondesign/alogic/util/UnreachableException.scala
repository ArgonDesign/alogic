////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Thrown when unreachable code is well, reached
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.util

case class UnreachableException()
    extends Exception(
      """Unreachable code was executed. Please file a bug report.
        |
        |                          ____/ (  (    )   )  \
        |                         /( (  (  )   _    ))  )   )\
        |                       ((     (   )(    )  )   (   )  )
        |                     ((/  ( _(   )   (   _) ) (  () )  )
        |                    ( (  ( (_)   ((    (   )  .((_ ) .  )_
        |                   ( (  )    (      (  )    )   ) . ) (   )
        |                  (  (   (  (   ) (  _  ( _) ).  ) . ) ) ( )
        |                  ( (  (   ) (  )   (  ))     ) _)(   )  )  )
        |                 ( (  ( \ ) (    (_  ( ) ( )  )   ) )  )) ( )
        |                  (  (   (  (   (_ ( ) ( _    )  ) (  )  )   )
        |                 ( (  ( (  (  )     (_  )  ) )  _)   ) _( ( )
        |                  ((  (   )(    (     _    )   _) _(_ (  (_ )
        |                   (_((__(_(__(( ( ( |  ) ) ) )_))__))_)___)
        |                   ((__)        \\||lll|l||///          \_))
        |                                /(/ (  )  ) )\
        |                               ( ( ( | | ) ) )\
        |                               /(| / ( )) ) ) ))
        |                               ( ((((_(|)_)))))
        |                                 ||\(|(|)|/||
        |                                 |(||(||)||||
        |                                //|/l|||)|\\ \
        |                        (/ / //  /|//||||\\  \ \  \ _)
        |-------------------------------------------------------------------------------
        |""".stripMargin
    )
