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

import org.eolang.AtComposite;
import org.eolang.AtCompositeTest;
import org.eolang.AtVoid;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.PhDefault;
import org.eolang.PhSafe;
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
final class EOtryTest {

    @Test
    void catchesException() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(
                new PhWith(
                    new PhWith(
                        new PhWith(
                            new EOtry(),
                            0, new PhSafe(new Broken())
                        ),
                        1, new Catcher()
                    ),
                    2,
                    new Data.ToPhi(true)
                )
            ).asString(),
            Matchers.containsString("it is brok")
        );
    }

    @Test
    void usesCatcherOutput() {
        final Phi body = new PhWith(
            new PhWith(
                new PhWith(
                    new EOtry(),
                    0, new PhSafe(new Broken())
                ),
                1, new Catcher()
            ),
            2,
            new Data.ToPhi(true)
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(body).asString(),
            Matchers.containsString("it is broken")
        );
    }

    @Test
    void worksWithoutException() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(
                new PhWith(
                    new PhWith(
                        new PhWith(
                            new EOtry(),
                            0, new Main()
                        ),
                        1, new Catcher()
                    ),
                    2,
                    new Data.ToPhi(true)
                )
            ).asNumber(),
            Matchers.equalTo(42.0)
        );
    }

    @Test
    void doesNotDataizeBodyTwice() {
        final Phi trier = new EOtry();
        final MainWithCounter main = new MainWithCounter();
        trier.put(0, main);
        trier.put(1, new Catcher());
        trier.put(2, new Data.ToPhi(true));
        new Dataized(trier).take();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            main.count,
            Matchers.equalTo(1)
        );
    }

    /**
     * Body object with counter.
     * @since 0.36.0
     */
    private static class MainWithCounter extends PhDefault {
        /**
         * Counter.
         */
        private int count;

        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        MainWithCounter() {
            super();
            this.add(
                Attr.PHI,
                new AtComposite(
                    this,
                    rho -> {
                        ++this.count;
                        return new Data.ToPhi(1L);
                    }
                )
            );
        }
    }

    /**
     * Main.
     * @since 1.0
     */
    private static class Main extends PhDefault {

        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Main() {
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
    private static class Broken extends PhDefault {
        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Broken() {
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
    private static class Catcher extends PhDefault {
        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Catcher() {
            this.add("ex", new AtVoid("ex"));
            this.add(
                "φ",
                new AtComposite(
                    this,
                    self -> self.take("ex")
                )
            );
        }
    }
}
