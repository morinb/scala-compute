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

/**
 *
 * @author morinb.
 */
object ANSIColors {
  val ESC_START = "\u001b["
  val ESC_END = "m"

  object Style {
    val Normal: String = "0"
    val Bright: String = "1"
    val Underline: String = "4"
    val Negative: String = "7"
  }

  object Foreground {
    val BlackForeground: String = "30"
    val RedForeground: String = "31"
    val GreenForeground: String = "32"
    val YellowForeground: String = "33"
    val BlueForeground: String = "34"
    val MagentaForeground: String = "35"
    val CyanForeground: String = "36"
    val WhiteForeground: String = "37"
    val DefaultForeground: String = "39"

  }

  object Background {
    val BlackBackground: String = "40"
    val RedBackground: String = "41"
    val GreenBackground: String = "42"
    val YellowBackground: String = "43"
    val BlueBackground: String = "44"
    val MagentaBackground: String = "45"
    val CyanBackground: String = "46"
    val WhiteBackground: String = "47"
    val DefaultBackground: String = "49"
  }

  def getColor(style: String = Style.Normal, foreground: String = Foreground.DefaultForeground, background: String = Background.DefaultBackground): String =
    s"$style;$foreground;$background"


}
