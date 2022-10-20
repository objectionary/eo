/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

import EOorg.EOeolang.EOio.EOstdout;
import EOorg.EOeolang.EOtxt.EOsprintf;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhCopy;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOseq}.
 *
 * @since 0.16
 */
public final class EOseqTest {
    /**
     * REDIRECTED_STDOUT.
     */
    private ByteArrayOutputStream redirected = new ByteArrayOutputStream();

    /**
     * DEFAULT_STDOUT.
     */
    private final PrintStream original = System.out;

    @Test
    public void calculatesAndReturns() {
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new PhWith(
                        new EOseq(Phi.Φ),
                        0, new Data.ToPhi(0L)
                    ),
                    0, new Data.ToPhi(1L)
                )
            ).take(Long.class),
            Matchers.equalTo(1L)
        );
    }

    @Test
    public void calculatesAndReturnsObject() {
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new PhWith(
                        new EOseq(Phi.Φ),
                        0, new Data.ToPhi(0L)
                    ),
                    0,
                    new PhWith(
                        new EOsprintf(Phi.Φ),
                        0, new Data.ToPhi("Hello!")
                    )
                )
            ).take(String.class),
            Matchers.startsWith("Hello")
        );
    }

    @Test
    public void makesTrueCopy() {
        final Phi first = new EOseq(Phi.Φ);
        first.attr(0).put(new Data.ToPhi(1L));
        final Phi second = first.copy();
        second.attr(0).put(new Data.ToPhi(2L));
        MatcherAssert.assertThat(
            new Dataized(first).take(Long.class),
            Matchers.equalTo(1L)
        );
        MatcherAssert.assertThat(
            new Dataized(second).take(Long.class),
            Matchers.equalTo(2L)
        );
    }

    @Test
    public void singleDataizationChecking() {
        System.setOut(new PrintStream(this.redirected));
        final String message = "祝你有美好的一天";
        final Phi output = new PhWith(
            new PhCopy(new EOstdout(Phi.Φ)),
            "text",
            new Data.ToPhi(message)
        );
        final Phi left = new Data.ToPhi(0L);
        final Phi right = new Data.ToPhi(1L);
        final Phi less = left.attr("lt").get();
        less.attr(0).put(right);
        final Phi sequence = new EOseq(Phi.Φ);
        sequence.attr(0).put(output);
        sequence.attr(1).put(less);
        MatcherAssert.assertThat(
            new Dataized(sequence).take(Boolean.class),
            Matchers.equalTo(true)
        );
        MatcherAssert.assertThat(
            this.redirected.toString(),
            Matchers.equalTo(message)
        );
        System.setOut(this.original);
    }
}
