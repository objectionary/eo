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
package org.eolang.compiler;

import java.io.IOException;
import java.util.Arrays;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Test;

/**
 * Test case for {@link DemoCommand}.
 *
 * @author John Page (johnpagedev@gmail.com)
 * @version $Id$
 * @since 0.1
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public final class DemoCommandTest {

    /**
     * Test.
     *
     * @throws Exception If some problem inside
     */
    @Test
    public void withNoCommandArgumentOutputsInstructions() throws Exception {
        final EocCommandArgument argument =
            new EocCommandArgument(new String[]{});
        MatcherAssert.assertThat(
            argument.isEmpty(),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            new DemoCommand(argument).output(),
            Matchers.containsString(
                "Use --demo <filename> to see one of the following"
            )
        );
    }

    /**
     * Test.
     *
     * @throws Exception If some problem inside
     */
    @Test
    public void withNoCommandArgumentOutputsTheListOfEoFiles()
        throws Exception {
        final EocCommandArgument argument =
            new EocCommandArgument(new String[]{});
        MatcherAssert.assertThat(
            argument.isEmpty(),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            new DemoCommand(argument).output(),
            Matchers.stringContainsInOrder(
                Arrays.asList("book.eo", "car.eo", "zero.eo")
            )
        );
    }

    /**
     * Test.
     *
     * @throws Exception If some problem inside
     */
    @Test(expected = IOException.class)
    public void withACommandArgumentForANonexistentFileThrows()
        throws Exception {
        final EocCommandArgument file =
            new EocCommandArgument(new String[]{"name", "doesNot.exist"});
        new DemoCommand(file).output();
    }
}
