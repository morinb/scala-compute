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

import org.scalatest.FunSuite


/**
 *
 * @author morinb.
 */
class ProcessTest extends FunSuite {

  test("sqrt(a^2+b^2)") {
    val formula = "sqrt(a^2+b^2)"
    val map = Map("a" -> "3", "b" -> "4")

    val expected = "5.0"

    assert(expected === Compute.compute(formula, map))
  }


  test("sqrt((1/4)*(m*g)^2) + log(10) - exp(0)") {
    val formula = "sqrt((1/4)*(m*g)^2) + log(10) - exp(0)"
    implicit val map = Map("m" -> "3", "g" -> "4")
    val expected = "6.0"

    assert(expected === Compute.compute(formula, map))
  }

  test("3^10^2") {
    val formula = "3^10^2"
    val expected = Math.pow(3, Math.pow(10, 2)).toString

    assert(expected === Compute.compute(formula, Map()))
  }

  test("(3^10)^2") {
    val formula = "(3^10)^2"
    val expected = Math.pow(Math.pow(3, 10), 2).toString

    assert(expected === Compute.compute(formula, Map()))
  }

  test("(-1) + (-2)") {
    val formula = "(-1) + (-2)"
    val expected = "-3.0"

    assert(expected === Compute.compute(formula, Map()))
  }

  test("3+ (-1) + (-2)") {
    val formula = "(-1) + (-2)"

    assert("0.0" === Compute.compute(s"3+(${Compute.compute(formula, Map())})", Map()))
  }

  test("x^2-x-1 | x = (sqrt(5)+1)/2") {
    val formula = "x^2-x-1"
    val vars = Map("x" -> "(sqrt(5)+1)/2")

    assert("0.0" === Compute.compute(formula, vars))
  }
  test("x^2-x-1 | x = phi") {
    val formula = "x^2-x-1"
    val vars = Map("x" -> "phi")

    assert("0.0" === Compute.compute(formula, vars))
  }


}
