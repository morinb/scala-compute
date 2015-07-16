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

package org.bm.scalacompute.lexer

import org.scalatest.FunSuite

/**
 *
 * @author morinb.
 */
class LexerTest extends FunSuite {

  test("testFormat") {
    val formula = "(-3)+x*2/(Z0-5 )^2^y'"
    val expected = "( - 3 ) + x * 2 / ( Z0 - 5 ) ^ 2 ^ y'"
    val lexer = new Lexer {
      override def parse(formula: String): List[Token] = Nil
    }

    val result = lexer.format(formula)

    assert(expected === result)

  }

}
