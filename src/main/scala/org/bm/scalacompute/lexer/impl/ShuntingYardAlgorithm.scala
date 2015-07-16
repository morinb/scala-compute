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

package org.bm.scalacompute.lexer.impl

import org.bm.scalacompute.Log
import org.bm.scalacompute.exception.MathematicalAnalysisException
import org.bm.scalacompute.lexer.{Lexer, _}

import scala.collection.mutable

/**
 *
 * @author morinb.
 */
class ShuntingYardAlgorithm(val variablesMap: Map[String, String]) extends Lexer with Log {
  override def parse(formula: String): List[String] = {
    if (log.isDebugEnabled) {
      log.debug(s"Formula: $formula")
    }
    val formattedFormula = format(formula)
    if (log.isDebugEnabled) {
      log.debug(s"Formatted formula: $formattedFormula")
    }


    analyze(formattedFormula.split(" "))

  }

  private[this] def analyze(items: Seq[String]): List[String] = {
    var queue: List[String] = Nil
    val stack: mutable.Stack[String] = mutable.Stack()
    var lastItem: String = ""

    def isVariable(item: String): Boolean = variablesMap.contains(item)
    def isOperator(item: String): Boolean = Operators.operatorsWithName.exists(tuple => tuple._1.contains(item.toLowerCase))
    def isFunction(item: String): Boolean = Functions.functionsWithName.exists(tuple => tuple._1.contains(item.toLowerCase))
    def isNumber(item: String): Boolean = try {
      item.toDouble
      true
    } catch {
      case _: Throwable => false
    }

    items.foreach { item =>

      if (log.isDebugEnabled) {
        log.debug(s"Analyzing item '$item'")
      }
      if (isNumber(item)) {
        if (log.isDebugEnabled) {
          log.debug(s"Item '$item' is a number. adding to Queue")
        }
        queue = item :: queue
        lastItem = item

      } else if (isVariable(item)) {
        if (log.isDebugEnabled) {
          log.debug(s"Item '$item' is a variable. Adding to Queue")
        }
        variablesMap.get(item) match {
          case Some(value) => queue = if (value != null) parse(value) ::: queue else item :: queue
          case None => queue = item :: queue
        }

      } else if (isOperator(item)) {
        if (log.isDebugEnabled) {
          log.debug(s"Item '$item' is an operator")
        }
        val token: String = if ("(" == lastItem) {

          if ("-" == item) {
            // Not subtraction but negate operator
            if (log.isDebugEnabled) {
              log.debug(s"Item '$item' is the negate operator")
            }
            "_"
          } else {
            item
          }

        } else {
          item
        }

        if (stack.nonEmpty && isOperator(stack.top)) {

          val op1 = Operators(token)
          val peek = stack.top
          val op2 = Operators(peek)

          if ((op1.precedence <= op2.precedence && op1.leftAssociative) ||
            (op1.precedence < op2.precedence && op1.rightAssociative)) {
            if (log.isDebugEnabled) {
              if (op1.leftAssociative) {
                log.debug(s"Item '$token' priority is <= '$peek' priority and '$token' is left-associative")
              } else {
                log.debug(s"Item '$token' priority is <= '$peek' priority and '$token' is right-associative")
              }
              log.debug(s"Popping '$peek' from the stack, and adding it to the queue.")
            }

            queue = stack.pop() :: queue
          } else {
            if (op1.precedence > op2.precedence) {
              if (log.isDebugEnabled) {
                log.debug(s"Item '$token' priority is > '$peek' priority")
              }
            }
          }
        }
        if (log.isDebugEnabled) {
          log.debug(s"Pushing '$token' onto the stack")
        }
        stack.push(token)
      } else if (isFunction(item)) {
        if (log.isDebugEnabled) {
          log.debug(s"Item '$item' is a function. Pushing onto the stack")
        }
        stack.push(item)
        lastItem = item
      } else if (isFunctionArgSeparator(item)) {
        if (log.isDebugEnabled) {
          log.debug(s"Item '$item' is a function arg separator")
        }

        while (stack.nonEmpty && "(" != stack.top) {
          val pop = stack.pop()
          if (log.isDebugEnabled) {
            log.debug(s"Pop '$pop' from the stack, adding it to the queue")
          }
          queue = pop :: queue
          if (stack.isEmpty) {
            throw new MathematicalAnalysisException("Error A: parenthesis problem")
          }
        }
        lastItem = item
      } else if ("(" == item) {
        if (log.isDebugEnabled) {
          log.debug(s"Pushing '$item' onto the stack")
        }
        stack.push(item)
        lastItem = item

      } else if (")" == item) {
        if (log.isDebugEnabled) {
          log.debug(s"Until ( is found on the stack, pop token from the stack to the queue")
        }
        while ("(" != stack.top) {
          val pop = stack.pop()
          if (log.isDebugEnabled) {
            log.debug(s"\tAdding '$pop' to the queue")
          }
          queue = pop :: queue
        }
        if (log.isDebugEnabled) {
          log.debug(s"( found. Dismiss from the stack.")
          stack.pop()
        }
        if (stack.nonEmpty && isFunction(stack.top)) {
          val peek = stack.top
          if (log.isDebugEnabled) {
            log.debug(s"Item '$peek' is a function, pop it from the stack to the queue")
          }
          queue = stack.pop() :: queue
        }
      } else {
        if (log.isDebugEnabled) {
          log.debug(s"Item '$item' unknown. Maybe a variable ? Added to queue")
        }
        queue = item :: queue
      }
      lastItem = item
    }


    if (log.isDebugEnabled) {
      log.debug(s"No more item to read")
    }

    while (stack.nonEmpty) {
      if ("(" == stack.top) {
        throw new MathematicalAnalysisException("Error C : parenthesis problem.")
      }
      val pop = stack.pop()
      if (log.isDebugEnabled) {
        log.debug(s"Popping '$pop' from the stack to the queue")
      }
      queue = pop :: queue
    }
    queue.reverse
  }

}
