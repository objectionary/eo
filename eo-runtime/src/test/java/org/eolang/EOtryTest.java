/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOtry}.
 * @since 0.19
 */
final class EOtryTest {

    @Test
    void catchesException() {
        MatcherAssert.assertThat(
            "catches exception",
            new Dataized(
                new PhApplication(
                    new PhApplication(
                        new PhApplication(
                            new EOtry(),
                            0, new PhSafe(new EOtryTest.Broken())
                        ),
                        1, new EOtryTest.Catcher()
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
        MatcherAssert.assertThat(
            "uses catcher's output",
            new Dataized(
                new PhApplication(
                    new PhApplication(
                        new PhApplication(
                            new EOtry(),
                            0, new PhSafe(new EOtryTest.Broken())
                        ),
                        1, new EOtryTest.Catcher()
                    ),
                    2,
                    new Data.ToPhi(true)
                )
            ).asString(),
            Matchers.containsString("it is broken")
        );
    }

    @Test
    void worksWithoutException() {
        MatcherAssert.assertThat(
            "Main threw an exception",
            new Dataized(
                new PhApplication(
                    new PhApplication(
                        new PhApplication(
                            new EOtry(),
                            0, new EOtryTest.Main()
                        ),
                        1, new EOtryTest.Catcher()
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
        final EOtryTest.MainWithCounter main = new EOtryTest.MainWithCounter();
        trier.put(0, main);
        trier.put(1, new EOtryTest.Catcher());
        trier.put(2, new Data.ToPhi(true));
        new Dataized(trier).take();
        MatcherAssert.assertThat(
            "EOorg.EOeolang.EOtry dataized body more than once",
            main.count,
            Matchers.equalTo(1)
        );
    }

    /**
     * Body object with counter.
     * @since 0.36.0
     */
    private static final class MainWithCounter extends PhDefault {

        /**
         * Counter.
         */
        private int count;

        /**
         * Ctor.
         * @checkstyle ConstructorsCodeFreeCheck (15 lines)
         */
        MainWithCounter() {
            super();
            this.add(
                Phi.PHI,
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
     * @since 0.1.0
     */
    private static final class Main extends PhDefault {

        /**
         * Ctor.
         * @checkstyle ConstructorsCodeFreeCheck (15 lines)
         */
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
     * @since 0.1.0
     */
    private static final class Broken extends PhDefault {

        /**
         * Ctor.
         * @checkstyle ConstructorsCodeFreeCheck (15 lines)
         */
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
     * @since 0.1.0
     */
    private static final class Catcher extends PhDefault {

        /**
         * Ctor.
         * @checkstyle ConstructorsCodeFreeCheck (15 lines)
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
