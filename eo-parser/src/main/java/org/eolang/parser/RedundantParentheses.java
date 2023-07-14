/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.parser;

import com.jcabi.log.Logger;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.regex.Pattern;

/**
 * The class that checks redundant parentheses for object expression.
 *
 * @since 0.28.12
 */
final class RedundantParentheses implements Predicate<String> {

    /**
     * Pattern for string literals.
     */
    private static final Pattern PATTERN = Pattern.compile("(?ms)#.*?\\n|\"\"\".*?\"\"\"|\".*?\"");

    /**
     * The callback that will be called in case if redundant parentheses is found.
     */
    private final Consumer<String> reaction;

    /**
     * Constructor with default reaction that writes warning to the log.
     */
    RedundantParentheses() {
        this(s -> Logger.warn("Redundant parentheses", s));
    }

    /**
     * The main constructor.
     *
     * @param reaction Will be called in case if redundant parentheses is found.
     */
    RedundantParentheses(final Consumer<String> reaction) {
        this.reaction = reaction;
    }

    /**
     * Checks if the expression contains redundant parentheses.
     * Examples of expressions to check:
     * <pre>{@code
     *  1.plus 2 > x
     *  "Text" > y
     *  (1.plus 2).plus 3
     * }
     * </pre>
     * @param expression Raw object expression from parser.
     * @return True if the expression contains redundant parentheses.
     */
    public boolean test(final String expression) {
        final Deque<Character> stack = new ArrayDeque<>();
        boolean res = false;
        for (final char symbol : RedundantParentheses.expressionChars(expression)) {
            if (symbol == ')') {
                boolean operation = false;
                char current = stack.pop();
                while (current != '(') {
                    if (current == '.' || current == ' ' || current == '-') {
                        operation = true;
                    }
                    current = stack.pop();
                }
                if (!operation) {
                    res = true;
                }
            } else {
                stack.push(symbol);
            }
        }
        if (stack.isEmpty()) {
            res = true;
        }
        if (res) {
            this.reaction.accept(expression);
        }
        return res;
    }

    /**
     * Clears raw expression from text literals and returns it as an array of chars.
     * @param expression Raw expression
     * @return Expression as an array of chars.
     * @todo #1897:30m Refactor regexp in `RedundantParenthesis`.
     *  The Java regex engine uses recursive method calls to implement backtracking.
     *  Therefore when a repetition inside a regular expression contains multiple paths
     *  (i.e. the body of the repetition contains an alternation (|), an optional
     *  element or another repetition), trying to match the regular expression can cause a
     *  stack overflow on large inputs. This does not happen when using a possessive quantifier
     *  (such as *+ instead of *) or when using a character class inside a repetition
     *  (e.g. [ab]* instead of (a|b)*).
     *  Refactor this repetition that can lead to a stack overflow for large inputs. (line 113)
     */
    private static char[] expressionChars(final String expression) {
        return RedundantParentheses
            .PATTERN.matcher(expression).replaceAll("literal").toCharArray();
    }
}
