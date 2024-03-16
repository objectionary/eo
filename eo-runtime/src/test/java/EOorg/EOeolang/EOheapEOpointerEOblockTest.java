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
 */
package EOorg.EOeolang;

import java.nio.charset.StandardCharsets;
import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOheap$EOpointer$EOblock}.
 *
 * @since 0.19
 * @checkstyle TypeNameCheck (4 lines)
 */
final class EOheapEOpointerEOblockTest {

    @Test
    void writesBytesIntoHeap() {
        final Phi heap = new PhWith(new EOheap(Phi.Φ), 0, new Data.ToPhi(100L));
        final Phi pointer = new PhWith(
            new PhMethod(heap, "pointer"),
            0, new Data.ToPhi(10L)
        );
        final String text = "Hello, друг!";
        final byte[] bytes = text.getBytes(StandardCharsets.UTF_8);
        final Phi block = new PhWith(
            new PhWith(
                new PhMethod(pointer, "block"),
                0, new Data.ToPhi((long) bytes.length)
            ),
            1, new EOheapEOpointerEOblockTest.Inverse(Phi.Φ)
        );
        new Dataized(
            new PhWith(
                new PhMethod(block, "write"),
                0, new Data.ToPhi(bytes)
            )
        ).take(Boolean.class);
        MatcherAssert.assertThat(
            new Dataized(block).take(String.class),
            Matchers.equalTo(text)
        );
    }

    /**
     * Inverse Phi.
     * @since 1.0
     */
    private static final class Inverse extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Inverse(final Phi sigma) {
            super(sigma);
            this.add("b", new AtFree());
            this.add(
                "φ",
                new AtComposite(
                    this,
                    self -> new Data.ToPhi(
                        new String(
                            new Dataized(self.attr("b").get()).take(byte[].class),
                            StandardCharsets.UTF_8
                        )
                    )
                )
            );
        }
    }

}
