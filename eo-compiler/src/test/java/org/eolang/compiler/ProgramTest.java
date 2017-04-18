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

import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import org.apache.commons.io.IOUtils;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.Test;

/**
 * Test case for {@link Program}.
 *
 * @author Yegor Bugayenko (yegor256@gmail.com)
 * @version $Id$
 * @since 0.1
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public final class ProgramTest {

    /**
     * Test zero example, this object implements two interfaces.
     *
     * @throws Exception If some problem inside
     */
    @Test
    public void processZeroExample() throws Exception {
        final Program program = new Program(
            IOUtils.toString(
                this.getClass().getResourceAsStream("eo/zero.eo"),
                Charset.defaultCharset()
            )
        );
        final Path dir = Files.createTempDirectory("");
        program.save(new FileOutput(dir));
        MatcherAssert.assertThat(
            new String(
                Files.readAllBytes(dir.resolve(Paths.get("zero.java")))
            ),
            Matchers.stringContainsInOrder(
                Arrays.asList(
                    "public",
                    "final",
                    "class",
                    "zero",
                    "implements",
                    "Money",
                    ",",
                    "Int",
                    "{",
                    "private final Int amount;",
                    "private final Text currency;",
                    "}"
                )
            )
        );
    }

    /**
     * Program can parse a simple fibonacci example.
     *
     * @throws Exception If some problem inside
     */
    @Test
    public void parsesFibonacciExample() throws Exception {
        final Program program = new Program(
            IOUtils.toString(
                this.getClass().getResourceAsStream("eo/fibonacci.eo"),
                Charset.defaultCharset()
            )
        );
        final Path dir = Files.createTempDirectory("");
        program.save(new FileOutput(dir));
        MatcherAssert.assertThat(
            new String(
                Files.readAllBytes(dir.resolve(Paths.get("fibonacci.java")))
            ),
            Matchers.stringContainsInOrder(
                Arrays.asList(
                    "public", "final", "class",
                    "fibonacci", "implements", "Int", "{",
                    "private final Int n;",
                    "public fibonacci()", "{",
                    "this(1);",
                    "}",
                    "public fibonacci(final Int n)", "{",
                    "this.n = n",
                    "}",
                    "}"
                )
            )
        );
    }

    /**
     * Program can parse a simple type with one method.
     *
     * @throws Exception If some problem inside
     */
    @Test
    public void parsesSimpleType() throws Exception {
        final Program program = new Program(
            IOUtils.toString(
                this.getClass().getResourceAsStream("eo/book.eo"),
                Charset.defaultCharset()
            )
        );
        final Path dir = Files.createTempDirectory("");
        program.save(new FileOutput(dir));
        MatcherAssert.assertThat(
            new String(
                Files.readAllBytes(dir.resolve(Paths.get("Book.java")))
            ),
            Matchers.allOf(
                Matchers.containsString("interface Book"),
                Matchers.containsString("Text text()")
            )
        );
    }

    /**
     * Program can parse a type with one method with parameters.
     *
     * @throws Exception If some problem inside
     */
    @Test
    public void parsesTypeWithParametrizedMethods() throws Exception {
        final Program program = new Program(
            IOUtils.toString(
                this.getClass().getResourceAsStream("eo/pixel.eo"),
                Charset.defaultCharset()
            )
        );
        final Path dir = Files.createTempDirectory("");
        program.save(new FileOutput(dir));
        MatcherAssert.assertThat(
            new String(
                Files.readAllBytes(dir.resolve(Paths.get("Pixel.java")))
            ),
            Matchers.allOf(
                Matchers.containsString("interface Pixel"),
                Matchers.containsString(
                    "Pixel moveTo(final Integer x, final Integer y)"
                )
            )
        );
    }

    /**
     * Program can parse a type with multiple methods.
     *
     * @throws Exception If some problem inside
     */
    @Test
    public void parsesBigType() throws Exception {
        final Program program = new Program(
            IOUtils.toString(
                this.getClass().getResourceAsStream("eo/car.eo"),
                Charset.defaultCharset()
            )
        );
        final Path dir = Files.createTempDirectory("");
        program.save(new FileOutput(dir));
        MatcherAssert.assertThat(
            new String(
                Files.readAllBytes(dir.resolve(Paths.get("Car.java")))
            ),
            Matchers.allOf(
                Matchers.containsString("interface Car"),
                Matchers.containsString("Money cost()"),
                Matchers.containsString("Bytes picture()"),
                Matchers.containsString("Car moveTo(final Coordinates coords)")
            )
        );
    }

    /**
     * Program can parse multiple types in one file.
     * This test verifies indentation parsing.
     *
     * @throws Exception If some problem inside
     */
    @Test
    public void parsesMultipleTypes() throws Exception {
        final Program program = new Program(
            IOUtils.toString(
                this.getClass().getResourceAsStream("eo/multitypes.eo"),
                Charset.defaultCharset()
            )
        );
        final Path dir = Files.createTempDirectory("");
        program.save(new FileOutput(dir));
        MatcherAssert.assertThat(
            new String(
                Files.readAllBytes(dir.resolve(Paths.get("Number.java")))
            ),
            Matchers.allOf(
                Matchers.containsString("interface Number"),
                Matchers.containsString("Decimal decimal()"),
                Matchers.containsString("Integral integral()")
            )
        );
        MatcherAssert.assertThat(
            new String(
                Files.readAllBytes(dir.resolve(Paths.get("Text.java")))
            ),
            Matchers.allOf(
                Matchers.containsString("interface Text"),
                Matchers.containsString("Number length()"),
                Matchers.containsString("Collection lines()")
            )
        );
    }
}
