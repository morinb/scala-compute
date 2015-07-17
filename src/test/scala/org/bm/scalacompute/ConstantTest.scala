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

package org.bm.scalacompute

import org.bm.scalacompute.Constants.{E, GAMMA, PHI, PI}
import org.bm.scalacompute.Implicits._
import org.scalatest.FunSuite

/**
 *
 * @author morinb.
 */
class ConstantTest extends FunSuite {
  val pi: Double = PI
  val e: Double = E
  val phi: Double = PHI
  val gamma: Double = GAMMA

  val threshold: Double = 0.00005

  test("pi") {
    assert(Math.PI === pi)
  }

  test("e") {
    assert(Math.E === e)
  }

  test("phi") {
    assert(((Math.sqrt(5) + 1) / 2) === phi)
  }

  test("gamma") {
    assert(0.5772156649015329 === gamma)
  }

  test("phi**2 = 1+phi") {
    val phi2 = Math.pow(phi, 2)
    val phi1 = 1 + phi

    assert(phi1 === phi2)
  }

  test("pi/6+phi**2 ~ pi") {
    val pi_6 = pi / 6
    val phi2 = 1 + phi // See test above

    assert(pi_6 + phi2 - pi < threshold)
  }

}
