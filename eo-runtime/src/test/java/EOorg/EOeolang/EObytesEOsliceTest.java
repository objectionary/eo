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

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.PhWith;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EObytes}.
 *
 * @since 0.23
 * @checkstyle TypeNameCheck (4 lines)
 */
final class EObytesEOsliceTest {

    @Test
    void takesLegalSlice() {
        MatcherAssert.assertThat(
            "slice is taken correctly",
            new Dataized(
                new PhWith(
                    new PhWith(
                        new Data.ToPhi("hello, world!")
                            .take("as-bytes")
                            .take("slice")
                            .copy(),
                        "start",
                        new Data.ToPhi(2)
                    ),
                    "len",
                    new Data.ToPhi(5)
                )
            ).asString(),
            Matchers.equalTo("llo, ")
        );
    }

    @Test
    void takesWrongSlice() {
        final ExFailure exp = Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(
                new PhWith(
                    new PhWith(
                        new Data.ToPhi("hello, world!")
                            .take("as-bytes")
                            .take("slice")
                            .copy(),
                        "start",
                        new Data.ToPhi(2)
                    ),
                    "len",
                    new Data.ToPhi(-5)
                )
            ).asString(),
            "fails on check"
        );
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try (PrintWriter writer = new PrintWriter(baos)) {
            exp.printStackTrace(writer);
        }
        MatcherAssert.assertThat(
            "error message is correct",
            baos.toString(),
            Matchers.allOf(
                Matchers.containsString("the 'len' attribute must be a positive integer"),
                Matchers.containsString("the 'len' attribute (-5)")
            )
        );
    }

    @Test
    void takesTooManyBytes() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(
                new PhWith(
                    new PhWith(
                        new Data.ToPhi("abc")
                            .take("as-bytes")
                            .take("slice")
                            .copy(),
                        "start",
                        new Data.ToPhi(0)
                    ),
                    "len",
                    new Data.ToPhi(100)
                )
            ).asString(),
            "fails because not enough bytes"
        );
    }

}
