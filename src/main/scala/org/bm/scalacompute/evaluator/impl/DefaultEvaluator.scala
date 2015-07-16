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

import org.bm.scalacompute.Implicits.enrichStack
import org.bm.scalacompute._
import org.bm.scalacompute.evaluator.Evaluator
import org.bm.scalacompute.exception.MathematicalAnalysisException

import scala.collection.mutable

/**
 *
 * @author morinb.
 */
class DefaultEvaluator extends Evaluator {
  override def eval(items: List[String]): String = {
    val stack: mutable.Stack[String] = new mutable.Stack()


    items foreach { item =>

      if (isFunction(item)) {
        val function = Functions(item)
        stack.push(function.executeMethod(stack.popx(function.argsNumber).reverse.toList))
      } else if (isOperator(item)) {
        val operator = Operators(item)
        stack.push(operator.executeMethod(stack.popx(operator.argsNumber).reverse.toList))
      } else {
        stack.push(item)
      }


    }

    if (stack.size != 1) {
      throw new MathematicalAnalysisException(s"Some tokens $stack are still on the stack, though all the formula has been analyzed.")
    }

    stack.pop()

  }

}
