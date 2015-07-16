/*
 *     Copyright (C) 2015  morinb
 *     https://github.com/morinb
 *
 *     This library is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU Lesser General Public
 *     License as published by the Free Software Foundation; either
 *     version 2.1 of the License, or (at your option) any later version.
 *
 *     This library is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *     Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public
 *     License along with this library; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

package org.bm.scalacompute.logback

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.pattern.color.ForegroundCompositeConverterBase
import org.bm.scalacompute.logback.ANSIColors.Style.Bright
import org.bm.scalacompute.logback.ANSIColors.Foreground._
import org.bm.scalacompute.logback.ANSIColors.Background._

/**
 *
 * @author 408658.
 */
class HighlightCompositeConverterEx extends ForegroundCompositeConverterBase[ILoggingEvent] {
  override def getForegroundColorCode(e: ILoggingEvent): String = e.getLevel.toInt match {
    case Level.ERROR_INT => ANSIColors.getColor(Bright, WhiteForeground, RedBackground)
    case Level.WARN_INT => ANSIColors.getColor(Bright, MagentaForeground)
    case Level.INFO_INT => ANSIColors.getColor(Bright, CyanForeground)
    case Level.DEBUG_INT => ANSIColors.getColor(Bright, YellowForeground)
    case Level.TRACE_INT => ANSIColors.getColor(Bright,WhiteForeground)
    case _ => ANSIColors.getColor()
  }
}
