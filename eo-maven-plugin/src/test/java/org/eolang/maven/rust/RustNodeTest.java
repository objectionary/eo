/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
package org.eolang.maven.rust;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;
import org.eolang.maven.WeHaveCargo;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link RustNode}.
 * @since 0.34
 */
final class RustNodeTest {

    @Test
    @ExtendWith(WeHaveCargo.class)
    void generatesRust(@TempDir final Path temp) throws IOException {
        final XML insert = new XMLDocument(
            "<rust code=\"75 73 65 20\" code_loc=\"Φ.org.eolang.custom-rust.r.α0\"><dependencies/></rust>"
        ).nodes("rust").get(0);
        final Path lib = temp.resolve("Lib");
        final Path  gen = temp.resolve("generated");
        new RustNode(
            insert,
            new Names(temp.resolve("names")),
            lib,
            Paths.get("../eo-runtime/src/main/rust/eo"),
            gen
        ).generate();
        final List<Path> out = Files.walk(temp).collect(Collectors.toList());
        MatcherAssert.assertThat(
            "Check that necessary files are created",
            out,
            Matchers.hasItems(
                lib.resolve("native0_QQ_custom_rust_r_0/src/foo.rs"),
                lib.resolve("native0_QQ_custom_rust_r_0/src/lib.rs"),
                gen.resolve("native0_QQ_custom_rust_r_0.java")
            )
        );
    }

    /**
     * Test. The cargo project to be compiled has incorrect dependency
     *  "nonexistent", so failure message should contain it.
     * @param temp Test directory.
     * @throws IOException if any issues with IO.
     * @checkstyle StringLiteralsConcatenationCheck (24 lines)
     * @checkstyle LineLengthCheck (24 lines)
     */
    @Test
    void printsCauseWhenFailsToBuild(@TempDir final Path temp) throws IOException {
        final XML insert = new XMLDocument(
            "<rust code=\"75 73 65 20 72 61 6E 64 3A 3A 52 6E 67 3B 0A 75 73 65 20 6E 6F 6E 65 78 69 73 74 65 6E 74 3A 3A 66 75 6E 0A 0A 70 75 62 20 66 6E 20 66 6F 6F 28 29 20 2D 3E 20 69 33 32 20 7B 0A 20 20 6C 65 74 20 6D 75 74 20 72 6E 67 20 3D 20 72 61 6E 64 3A 3A 74 68 72 65 61 64 5F 72 6E 67 28 29 3B 0A 20 20 70 72 69 6E 74 21 28 22 48 65 6C 6C 6F 20 77 6F 72 6C 64 22 29 3B 0A 20 20 6C 65 74 20 69 20 3D 20 72 6E 67 2E 67 65 6E 3A 3A 3C 69 33 32 3E 28 29 3B 0A 20 20 69 0A 7D\"\n"
                + "  code_loc=\"Φ.org.eolang.custom.incorrect-code.r.α0\">\n"
                + "  <dependencies>\n"
                + "    <dependency name=\"6E 6F 6E 65 78 69 73 74 65 6E 74 20 3D 20 2D 35\"/>\n"
                + "  </dependencies>\n"
                + "</rust>\n"
        ).nodes("rust").get(0);
        final Buildable node = new RustNode(
            insert,
            new Names(temp.resolve("names")),
            temp.resolve("Lib"),
            Paths.get("../eo-runtime/src/main/rust/eo"),
            temp.resolve("generated")
        );
        node.generate();
        MatcherAssert.assertThat(
            "It should print error message why cargo project failed",
            RustNodeTest.getExceptionMessage(
                () -> node.build(temp.resolve(".eo")),
                BuildFailureException.class
            ),
            Matchers.stringContainsInOrder(
                "Failed to build cargo project with dest",
                "nonexistent"
            )
        );
    }

    /**
     * Get message of exception to be thrown.
     * @param exec Something that should throw exception of specified type.
     * @param type Subtype of {@link Exception} to be thrown.
     * @return Message retrieved from exception.
     * @checkstyle IllegalCatchCheck (16 lines)
     */
    @SuppressWarnings("PMD.AvoidCatchingGenericException")
    private static String
        getExceptionMessage(final Runnable exec, final Class<? extends Exception> type) {
        try {
            exec.run();
            throw new IllegalStateException("Should have thrown an exception");
        } catch (final Exception ex) {
            if (type.isAssignableFrom(ex.getClass())) {
                return ex.getMessage();
            }
            throw new IllegalStateException(ex);
        }
    }
}
