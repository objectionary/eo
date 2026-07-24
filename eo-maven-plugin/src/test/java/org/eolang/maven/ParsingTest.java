/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Path;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.UnaryOperator;
import org.eolang.parser.Canonical;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Parsing}.
 * @since 0.61.0
 */
@ExtendWith(MktmpResolver.class)
final class ParsingTest {

    /**
     * Race a few threads of one {@link Parsing} on a single tail path.
     *
     * <p>This is the scenario of #5720: the {@link ConcurrentCache} guard is
     * shared by the whole instance, so the first thread compiles and the rest
     * wait for it and then read what it cached. When every file used to get a
     * guard of its own, each thread locked its own private map, all of them
     * missed the cache and the source was parsed as many times as there were
     * threads.</p>
     *
     * <p>The guard covers the write to the cache, not the read of the target
     * XMIR that follows it, and {@link Saved} truncates that file before
     * streaming into it. A thread can therefore read it half-written and get
     * "Premature end of file" (#5873), which is a different race from the one
     * under test, so it is counted and ignored here.</p>
     *
     * @param temp Temporary directory
     * @throws Exception If fails
     */
    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void parsesOnlyOnceWhenThreadsRaceOnTheSameTail(@Mktmp final Path temp) throws Exception {
        final Path source = temp.resolve("main.eo");
        new Saved(
            String.format("# Sample.%n[] > main%n"),
            source
        ).value();
        final TjsForeign tojos = new TjsForeign();
        final TjForeign tojo = tojos.add("main").withSource(source).withVersion(Parsing.ZERO);
        final Parsing parsing = new Parsing(
            tojos,
            temp.resolve("target"),
            temp.resolve("cache"),
            true,
            Parsing.ZERO,
            temp
        );
        final UnaryOperator<XML> canonical = new Canonical("main");
        final AtomicInteger parses = new AtomicInteger(0);
        final AtomicInteger torn = new AtomicInteger(0);
        new Threaded<>(
            Collections.nCopies(8, tojo),
            each -> {
                try {
                    parsing.parsed(
                        each,
                        xml -> {
                            parses.incrementAndGet();
                            return canonical.apply(xml);
                        },
                        "digest"
                    );
                } catch (final IllegalArgumentException half) {
                    torn.incrementAndGet();
                    Logger.debug(this, "Caught a half-written XMIR (#5873): %s", half);
                }
                return 1;
            }
        ).total();
        MatcherAssert.assertThat(
            "Threads racing on one tail path through one Parsing must parse the source once, the rest must be served from the cache",
            parses.get(),
            Matchers.equalTo(1)
        );
    }
}
