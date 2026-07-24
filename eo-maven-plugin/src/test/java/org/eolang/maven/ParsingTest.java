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
     * Race threads of one {@link Parsing} on a single tail path (#5720): the
     * first one compiles, the rest wait for its lock and read what it cached.
     * With a guard per file each thread locked its own empty map, all of them
     * missed and the source was parsed once per thread. The target is read
     * back after the guard is released, where {@link Saved} can be caught
     * mid-write (#5873), which is a different race and is ignored here.
     * @param temp Temporary directory
     * @throws Exception If fails
     */
    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void parsesOnlyOnceWhenThreadsRaceOnTheSameTail(@Mktmp final Path temp) throws Exception {
        final Path source = temp.resolve("main.eo");
        new Saved(String.format("# Sample.%n[] > main%n"), source).value();
        final TjsForeign tojos = new TjsForeign();
        final TjForeign tojo = tojos.add("main").withSource(source).withVersion(Parsing.ZERO);
        final Parsing parsing = new Parsing(
            tojos, temp.resolve("target"), temp.resolve("cache"), true, Parsing.ZERO, temp
        );
        final AtomicInteger parses = new AtomicInteger(0);
        final UnaryOperator<XML> pipeline = xml -> {
            parses.incrementAndGet();
            return new Canonical("main").apply(xml);
        };
        new Threaded<>(
            Collections.nCopies(8, tojo),
            each -> {
                try {
                    parsing.parsed(each, pipeline, "digest");
                } catch (final IllegalArgumentException half) {
                    Logger.debug(this, "Half-written XMIR (#5873): %s", half);
                }
                return 1;
            }
        ).total();
        MatcherAssert.assertThat(
            "Threads racing on one tail path must parse the source once, the rest must be cached",
            parses.get(),
            Matchers.equalTo(1)
        );
    }
}
