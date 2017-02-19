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
 * Test case for {@link EoResourceFiles}.
 *
 * @author John Page (johnpagedev@gmail.com)
 * @version $Id$
 * @since 0.1
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public final class EoResourceFilesTest {

    /**
     * An EoResourceFiles object returns list of EO resource file names.
     *
     * @throws Exception If there's an exception.
     */
    @Test
    public void returnsListOfEoResourceFileNames()
        throws Exception {
        MatcherAssert.assertThat(
            new EoResourceFiles("org/eolang/compiler/eo/")
                .formattedNames(),
            Matchers.stringContainsInOrder(
                Arrays.asList("book.eo", "zero.eo")
            )
        );
    }

    /**
     * An EoResourceFiles object returns an empty string
     * if no EO files are in the path.
     *
     * @throws Exception If there's an exception.
     */
    @Test
    public void returnsEmptyStringIfNoEoFilesAreInThePath()
        throws Exception {
        MatcherAssert.assertThat(
            new EoResourceFiles("org/eolang/compiler/")
                .formattedNames(),
            Matchers.is("")
        );
    }

    /**
     * An EoResourceFiles object with a nonexistent path throws while
     * requesting formatted names.
     *
     * @throws Exception If there's an exception.
     */
    @Test(expected = IOException.class)
    public void withANonexistentPathThrowsWhileRequestingFormattedNames()
        throws Exception {
        new EoResourceFiles("non/existent/path").formattedNames();
    }

    /**
     * An EoResourceFiles object return code from the specified EO file.
     *
     * @throws Exception If there's an exception.
     */
    @Test
    public void returnsCodeFromTheSpecifiedEoFile()
        throws Exception {
        MatcherAssert.assertThat(
            new EoResourceFiles("org/eolang/compiler/eo/")
                .eolang("pixel.eo"),
            Matchers.containsString("Pixel moveTo(Integer x")
        );
    }

    /**
     * An EoResourceFiles object with a nonexistent path throws while
     * requesting EO code.
     *
     * @throws Exception If there's an exception.
     */
    @Test(expected = IOException.class)
    public void withANonexistentPathThrowsWhileRequestingEoCode()
        throws Exception {
        new EoResourceFiles("non/existent/path")
            .eolang("book.eo");
    }

    /**
     * An EoResourceFiles object returns compiled Java for the
     * specified EO file.
     *
     * @throws Exception If there's an exception.
     */
    @Test
    public void returnsCompiledJavaForTheSpecifiedEoFile()
        throws Exception {
        MatcherAssert.assertThat(
            new EoResourceFiles("org/eolang/compiler/eo/")
                .java("pixel.eo"),
            Matchers.containsString("public interface Pixel {")
        );
    }

    /**
     * An EoResourceFiles object with a nonexistent path throws while
     * requesting compiled Java code.
     *
     * @throws Exception If there's an exception.
     */
    @Test(expected = IOException.class)
    public void withANonexistentPathThrowsWhileRequestingCompiledJava()
        throws Exception {
        new EoResourceFiles("non/existent/path")
            .java("book.eo");
    }
}
