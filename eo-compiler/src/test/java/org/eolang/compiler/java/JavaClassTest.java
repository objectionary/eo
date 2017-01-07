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
package org.eolang.compiler.java;

import java.util.Arrays;
import java.util.Collection;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Test;

/**
 * Java class generator test.
 *
 * @author Kirill (g4s8.public@gmail.com)
 * @version $Id$
 * @since 0.1
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public final class JavaClassTest {

    /**
     * Test class code.
     */
    @Test
    public void classCode() {
        final String name = "Cat";
        final String type = "Animal";
        MatcherAssert.assertThat(
            new JavaClass(
                name,
                type
            ).code(),
            Matchers.stringContainsInOrder(
                Arrays.asList(
                    "public",
                    "final",
                    "class",
                    name,
                    "implements",
                    type,
                    "{",
                    "}"
                )
            )
        );
    }

    /**
     * Test class with multiple interfaces.
     */
    @Test
    public void multiTypes() {
        final String name = "pdf";
        final Collection<String> types = Arrays.asList("Text", "Book");
        MatcherAssert.assertThat(
            new JavaClass(
                name,
                types
            ).code(),
            Matchers.stringContainsInOrder(
                Arrays.asList(
                    "public",
                    "final",
                    "class",
                    name,
                    "implements",
                    "Text", ",",
                    "Book",
                    "{", "}"
                )
            )
        );
    }

    /**
     * Test class path.
     */
    @Test
    public void classPath() {
        final String name = "Book";
        MatcherAssert.assertThat(
            new JavaClass(
                name,
                "Text"
            ).path().toString(),
            Matchers.equalTo(
                String.format(
                    "%s.java",
                    name
                )
            )
        );
    }
}
