/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.apache.log4j.Appender;
import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.LoggingEvent;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.api.parallel.ResourceLock;

/**
 * Test case for {@link Lowered}.
 *
 * <p>The logger of {@link Lowered} is one object for the whole JVM, and
 * a capture of it is a level and an appender set on that object, so two
 * of these tests in flight at once read each other's messages, or none
 * at all. They run in one thread for that reason.</p>
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
@Execution(ExecutionMode.SAME_THREAD)
@ResourceLock("org.eolang.lowering.Lowered.log")
final class LoweredTest {

    @Test
    void namesTheVoidNothingWitnesses(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the void no table witnesses must be named in the log, but it isnt",
            LoweredTest.spoken(
                new Formas(
                    Collections.singletonMap("Φ.foo.calc.φ", "Φ.number"),
                    Collections.emptyMap()
                ),
                String.join(
                    "",
                    "<object><o name='foo'><o loc='Φ.foo.calc' name='calc'>",
                    "<o base='∅' name='x'/><o base='Φ.number' name='φ'/>",
                    "</o></o></object>"
                ),
                temp
            ),
            Matchers.hasItem(
                Matchers.<String>allOf(
                    Matchers.containsString("'x'"),
                    Matchers.containsString("Φ.foo.calc")
                )
            )
        );
    }

    @Test
    void countsTheAttributesOfAFormationItCannotShape(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a formation of the wrong shape must be counted in the log, but it isnt",
            LoweredTest.spoken(
                new Formas(
                    Collections.emptyMap(),
                    Collections.singletonMap("Φ.foo.calc.x", "number")
                ),
                String.join(
                    "",
                    "<object><o name='foo'><o loc='Φ.foo.calc' name='calc'>",
                    "<o base='∅' name='x'/><o base='Φ.number' name='φ'/>",
                    "<o base='Φ.number' name='bar'/>",
                    "</o></o></object>"
                ),
                temp
            ),
            Matchers.hasItem(
                Matchers.<String>allOf(
                    Matchers.containsString("Φ.foo.calc"),
                    Matchers.containsString("3 attribute(s)")
                )
            )
        );
    }

    @Test
    void tellsTheReasonAFragmentRefused(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the reason a fragment did not lower must reach the log, but it doesnt",
            LoweredTest.spoken(
                new Formas(
                    Collections.emptyMap(),
                    Collections.singletonMap("Φ.foo.calc.x", "number")
                ),
                String.join(
                    "",
                    "<object><o name='foo'><o loc='Φ.foo.calc' name='calc'>",
                    "<o base='∅' name='x'/>",
                    "<o base='Φ.number' name='φ'><o as='α0' base='Φ.foo.calc.x'/></o>",
                    "</o></o></object>"
                ),
                temp
            ),
            Matchers.hasItem(
                Matchers.<String>allOf(
                    Matchers.containsString("Φ.foo.calc"),
                    Matchers.containsString("refused to lower")
                )
            )
        );
    }

    private static List<String> spoken(final Formas formas, final String xmir,
        final Path temp) throws IOException {
        final List<String> messages = new ArrayList<>(0);
        final Appender appender = new AppenderSkeleton() {
            @Override
            protected void append(final LoggingEvent event) {
                messages.add(String.valueOf(event.getRenderedMessage()));
            }

            @Override
            public void close() {
                // Nothing to release.
            }

            @Override
            public boolean requiresLayout() {
                return false;
            }
        };
        final Logger logger = Logger.getLogger(Lowered.class);
        final Level level = logger.getLevel();
        logger.setLevel(Level.ALL);
        logger.addAppender(appender);
        try {
            new Lowered(new Phino("phino-of-no-machine", 1, temp), formas, temp)
                .rewrite(new Xnav(xmir));
        } finally {
            logger.removeAppender(appender);
            logger.setLevel(level);
        }
        return messages;
    }
}
