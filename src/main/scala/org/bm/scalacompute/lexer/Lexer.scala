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

import org.bm.scalacompute.lexer.impl.ShuntingYardAlgorithm

/**
 *
 * @author morinb.
 */
trait Lexer {

  def parse(formula: String): List[String]

  def isFunctionArgSeparator(item: String): Boolean = ";" == item

  def format(formula: String): String = {
    var formatted = formula.replace("(", " ( ").replace(")", " ) ").replace(",", " , ")

    Operators.getOperators.foreach(operator =>
      operator.lowerCaseNames.foreach(name =>
        formatted = formatted.replace(name, s" $name ")
      )
    )

    formatted.replaceAll("\\s+", " ").trim

  }

}

object Lexer {
  def apply(variablesMap: Map[String, String]) = new ShuntingYardAlgorithm(variablesMap)
}