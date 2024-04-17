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

import org.eolang.AtComposite;
import org.eolang.AtOnce;
import org.eolang.AtSimple;
import org.eolang.AtVoid;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOtuple}.
 *
 * @since 0.1
 * @checkstyle TypeNameCheck (2 lines)
 */
final class EOtupleEOatTest {

    @Test
    void pushesAndGetsBack() {
        final String txt = "Hello, world!";
        final Phi str = new Data.ToPhi(txt);
        final Phi tuple = new PhWith(
            new EOtuple$EOempty(Phi.Φ).take("with").copy(),
            0, str
        );
        final Phi idx = new Data.ToPhi(0L);
        final Phi get = tuple.take("at").copy();
        get.put(0, idx);
        MatcherAssert.assertThat(
            "TO ADD ASSERTION MESSAGE",
            new Dataized(get).take(String.class),
            Matchers.equalTo(txt)
        );
        MatcherAssert.assertThat(
            "TO ADD ASSERTION MESSAGE",
            new Dataized(get).take(String.class),
            Matchers.equalTo(txt)
        );
    }

    @Test
    void checksNegativeIndex() {
        MatcherAssert.assertThat(
            "TO ADD ASSERTION MESSAGE",
            new Dataized(this.get(-1L)).take(String.class),
            Matchers.equalTo("second")
        );
        MatcherAssert.assertThat(
            "TO ADD ASSERTION MESSAGE",
            new Dataized(this.get(-2L)).take(String.class),
            Matchers.equalTo("first")
        );
    }

    @Test
    void checksOutOfBounds() {
        Assertions.assertThrows(
            EOerror.ExError.class,
            () -> new Dataized(this.get(-3L)).take(),
            "TO ADD ASSERTION MESSAGE"
        );
    }

    @Test
    void returnsGivenArgument() {
        final Phi tuple = new EOtuple(Phi.Φ);
        final Phi empty = tuple.take("empty");
        final Phi copy = tuple.copy();
        copy.put(0, empty);
        copy.put(1, new Data.ToPhi(10L));
        final Phi phi = new PhWith(
            new Parenting(Phi.Φ),
            "args", copy
        );
        MatcherAssert.assertThat(
            "TO ADD ASSERTION MESSAGE",
            new Dataized(phi).take(Long.class),
            Matchers.equalTo(10L)
        );
    }

    private Phi get(final long index) {
        final String first = "first";
        final String second = "second";
        final Phi tuple = new PhWith(
            new PhWith(
                new EOtuple$EOempty(Phi.Φ).take("with").copy(),
                0, new Data.ToPhi(first)
            ).take("with").copy(),
            0, new Data.ToPhi(second)
        );
        final Phi idx = new Data.ToPhi(index);
        final Phi get = tuple.take("at").copy();
        get.put(0, idx);
        return get;
    }

    /**
     * Parenting.
     * @since 0.36.0
     */
    private static class Parenting extends PhDefault {
        Parenting(final Phi sigma) {
            super(sigma);
            this.add("args", new AtVoid("args"));
            this.add("take", new AtSimple(new Take(this)));
            this.add(
                Attr.PHI,
                new AtOnce(
                    new AtComposite(this, rho -> new PhMethod(rho, "take"))
                )
            );
        }
    }

    /**
     * Take.
     * @since 0.36.0
     */
    private static class Take extends PhDefault {
        Take(final Phi sigma) {
            super(sigma);
            this.add(
                Attr.PHI,
                new AtComposite(
                    this,
                    rho -> {
                        final Phi ret = rho.take(Attr.RHO)
                            .take("args")
                            .take("at")
                            .copy();
                        ret.put(0, new Data.ToPhi(0L));
                        return ret;
                    }
                )
            );
        }
    }
}
