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

package org.bm.scalacompute.evaluator.impl

import org.bm.scalacompute.evaluator.Evaluator
import org.scalatest.FunSuite

/**
 *
 * @author morinb.
 */
class EvaluatorTest extends FunSuite {

  test("testEval") {
    val formula = List("3", "4", "-")
    val expected = "-1.0"

    assert(expected === Evaluator().eval(formula))
  }

  test("testEval_2") {
    val formula = List("10", "log")
    val expected = "1.0"

    assert(expected === Evaluator().eval(formula))
  }

}
