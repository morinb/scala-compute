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

import org.bm.scalacompute._
import org.bm.scalacompute.Operators._
import org.scalatest.FunSuite

/**
 *
 * @author morinb.
 */
class OperatorTest extends FunSuite {

  test("plus") {

    val operatorPlus: Operator = PLUS
    assert("3.0" === operatorPlus.executeMethod(List("1", "2")), "1+2=3")
  }

  test("minus") {
    assert("-1.0" === MINUS.executeMethod(List("1", "2")), "1-2=-1")
  }

  test("times") {
    assert("10.0" === TIMES.executeMethod(List("2.0", "5.0")), "2*5=10")
  }

  test("divide") {
    assert("2.0" === DIVIDE.executeMethod(List("10", "5")), "10/5=2")
  }

  test("negate") {
    assert("-1.0" === NEGATE.executeMethod(List("1")), "-1 = -1")
  }

  test("modulo") {
    assert("0.0" === MODULO.executeMethod(List("10", "1")), "10%1=0")
    assert("0.0" === MODULO.executeMethod(List("10", "2")), "10%2=0")
    assert("1.0" === MODULO.executeMethod(List("10", "3")), "10%3=1")
    assert("2.0" === MODULO.executeMethod(List("10", "4")), "10%4=2")
    assert("0.0" === MODULO.executeMethod(List("10", "5")), "10%5=0")
    assert("4.0" === MODULO.executeMethod(List("10", "6")), "10%6=4")
    assert("3.0" === MODULO.executeMethod(List("10", "7")), "10%7=3")
    assert("2.0" === MODULO.executeMethod(List("10", "8")), "10%8=2")
    assert("1.0" === MODULO.executeMethod(List("10", "9")), "10%9=1")
  }

  test("power") {
    assert("100.0" === POWER.executeMethod(List("10", "2")), "10**2=100")
  }

  test("+") {
    assert(Operators.PLUS === Operators("+"))
  }
  test("-") {
    assert(Operators.MINUS === Operators("-"))
  }
  test("~") {
    assert(Operators.NEGATE === Operators("~"))
  }

}
