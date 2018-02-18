/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016 eolang.org
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
package org.eolang.compiler.syntax;

import org.cactoos.list.StickyList;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Test;

/**
 * Test for object body.
 *
 * @author Kirill (g4s8.public@gmail.com)
 * @version $Id$
 * @since 0.1
 */
public final class ObjectBodyTest {

    /**
     * Java fields for object attributes and primary constructor.
     */
    @Test
    public void fieldsAndPrimaryCtorTest() {
        MatcherAssert.assertThat(
            new ObjectBody(
                new StickyList<>(
                    new Attribute("Number", "amount"),
                    new Attribute("Decimal", "price")
                ),
                new StickyList<>(),
                new StickyList<>()
            ).java("product"),
            Matchers.stringContainsInOrder(
                new StickyList<>(
                    "private final Number amount;",
                    "private final Decimal price;",
                    "public product(final Number amount, final Decimal price)",
                    "{",
                    "this.amount = amount;",
                    "this.price = price;",
                    "}"
                )
            )
        );
    }

    /**
     * Test for secondary constructor.
     */
    @Test
    @SuppressWarnings("PMD.AvoidDuplicateLiterals")
    public void secondaryConstructorTest() {
        MatcherAssert.assertThat(
            new ObjectBody(
                new StickyList<>(),
                new StickyList<>(
                    new Ctor(
                        new StickyList<>(
                            new Parameter("title", "Text")
                        ),
                        new StickyList<>(
                            new Argument.Fake("0"),
                            new Argument.Fake("title")
                        )
                    )
                ),
                new StickyList<>()
            ).java("book"),
            Matchers.stringContainsInOrder(
                new StickyList<>(
                    "public book(final Text title)", "{",
                    "this(0, title);",
                    "}"
                )
            )
        );
    }
}
