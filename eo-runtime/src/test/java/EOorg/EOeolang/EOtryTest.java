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
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.PhDefault;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOtry}.
 *
 * @since 0.19
 */
public final class EOtryTest {

    @Test
    public void catchesException() {
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new PhWith(
                        new PhWith(
                            new EOtry(Phi.Φ),
                            0, new Broken(Phi.Φ)
                        ),
                        1, new Catcher(Phi.Φ)
                    ),
                    2,
                    new EOnop(Phi.Φ)
                )
            ).take(String.class),
            Matchers.containsString("it is brok")
        );
    }

    @Test
    public void usesCatcherOutput() {
        final Phi body = new PhWith(
            new PhWith(
                new PhWith(
                    new EOtry(Phi.Φ),
                    0, new Broken(Phi.Φ)
                ),
                1, new Catcher(Phi.Φ)
            ),
            2,
            new EOnop(Phi.Φ)
        );
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new PhMethod(body, "eq"),
                    0, new Data.ToPhi("it is broken")
                )
            ).take(Boolean.class),
            Matchers.is(true)
        );
    }

    @Test
    public void printsCatcherOutput() {
        final Phi body = new PhWith(
            new PhWith(
                new PhWith(
                    new EOtry(Phi.Φ),
                    0, new Broken(Phi.Φ)
                ),
                1, new Catcher(Phi.Φ)
            ),
            2,
            new EOnop(Phi.Φ)
        );
        MatcherAssert.assertThat(
            new Dataized(body).take(String.class),
            Matchers.equalTo("it is broken")
        );
    }

    @Test
    public void worksWithoutException() {
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new PhWith(
                        new PhWith(
                            new EOtry(Phi.Φ),
                            0, new Main(Phi.Φ)
                        ),
                        1, new Catcher(Phi.Φ)
                    ),
                    2,
                    new EOnop(Phi.Φ)
                )
            ).take(Long.class),
            Matchers.equalTo(42L)
        );
    }

    /**
     * Main.
     * @since 1.0
     */
    public static class Main extends PhDefault {

        /**
         * Ctor.
         * @param sigma Sigma
         */
        public Main(final Phi sigma) {
            super(sigma);
            this.add(
                "φ",
                new AtComposite(
                    this,
                    self -> new Data.ToPhi(
                        new Dataized(new Data.ToPhi(42L)).take()
                    )
                )
            );
        }
    }

    /**
     * Broken.
     * @since 1.0
     */
    public static class Broken extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma.
         */
        public Broken(final Phi sigma) {
            super(sigma);
            this.add(
                "φ",
                new AtComposite(
                    this,
                    self -> {
                        throw new ExFailure("it is broken");
                    }
                )
            );
        }
    }

    /**
     * Catcher.
     * @since 1.0
     */
    public static class Catcher extends PhDefault {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        public Catcher(final Phi sigma) {
            super(sigma);
            this.add("ex", new AtFree());
            this.add(
                "φ",
                new AtComposite(
                    this,
                    self -> self.attr("ex").get()
                )
            );
        }
    }
}
