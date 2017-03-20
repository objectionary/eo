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

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Test;

/**
 * Test for simple constructor arguments, such as text, number, bool, etc.
 *
 * @author Kirill (g4s8.public@gmail.com)
 * @version $Id$
 * @since 0.1
 */
public final class ArgSimpleTest {

    /**
     * Simple text.
     */
    @Test
    public void text() {
        final String text = "\"text\"";
        MatcherAssert.assertThat(
            new ArgSimple(text).java(),
            Matchers.equalTo(text)
        );
    }

    /**
     * Simple integer.
     */
    @Test
    public void integer() {
        final int target = 42;
        MatcherAssert.assertThat(
            new ArgSimple(target).java(),
            Matchers.equalTo("42")
        );
    }

    /**
     * Simple float.
     */
    @Test
    public void flt() {
        final double target = 136.11;
        MatcherAssert.assertThat(
            new ArgSimple(target).java(),
            Matchers.equalTo("136.11")
        );
    }

    /**
     * Simple bool.
     */
    @Test
    public void bool() {
        MatcherAssert.assertThat(
            new ArgSimple(true).java(),
            Matchers.equalTo("true")
        );
    }
}
