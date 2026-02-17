/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import org.eolang.AtComposite;
import org.eolang.AtVoid;
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
            "catches exception",
            new Dataized(
                new PhWith(
                    new PhWith(
                        new PhWith(
                            new EOtry(),
                            0, new PhSafe(Broken.make())
                        ),
                        1, Catcher.make()
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
                new PhWith(
                    new PhWith(
                        new PhWith(
                            new EOtry(),
                            0, new PhSafe(Broken.make())
                        ),
                        1, Catcher.make()
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
                new PhWith(
                    new PhWith(
                        new PhWith(
                            new EOtry(),
                            0, Main.make()
                        ),
                        1, Catcher.make()
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
        final MainWithCounter main = MainWithCounter.make();
        trier.put(0, main);
        trier.put(1, Catcher.make());
        trier.put(2, new Data.ToPhi(true));
        new Dataized(trier).take();
        MatcherAssert.assertThat(
            "EOtry dataized body more than once",
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
         * Factory method.
         * @return New MainWithCounter instance
         */
        static MainWithCounter make() {
            final MainWithCounter main = new MainWithCounter();
            main.add(
                Phi.PHI,
                new AtComposite(
                    main,
                    rho -> {
                        ++main.count;
                        return new Data.ToPhi(1L);
                    }
                )
            );
            return main;
        }
    }

    /**
     * Main.
     * @since 0.1.0
     */
    private static final class Main extends PhDefault {

        /**
         * Factory method.
         * @return New Main instance
         */
        static Main make() {
            final Main main = new Main();
            main.add(
                "φ",
                new AtComposite(
                    main,
                    self -> new Data.ToPhi(
                        new Dataized(new Data.ToPhi(42L)).take()
                    )
                )
            );
            return main;
        }
    }

    /**
     * Broken.
     * @since 0.1.0
     */
    private static final class Broken extends PhDefault {
        /**
         * Factory method.
         * @return New Broken instance
         */
        static Broken make() {
            final Broken broken = new Broken();
            broken.add(
                "φ",
                new AtComposite(
                    broken,
                    self -> {
                        throw new ExFailure("it is broken");
                    }
                )
            );
            return broken;
        }
    }

    /**
     * Catcher.
     * @since 0.1.0
     */
    private static final class Catcher extends PhDefault {
        /**
         * Create a new Catcher.
         * @return New Catcher
         */
        static Catcher make() {
            final Catcher catcher = new Catcher();
            catcher.add("ex", new AtVoid("ex"));
            catcher.add(
                "φ",
                new AtComposite(
                    catcher,
                    self -> self.take("ex")
                )
            );
            return catcher;
        }
    }
}
