/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven.dealpha;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.IntStream;
import org.apache.log4j.Appender;
import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.LoggingEvent;
import org.cactoos.Scalar;
import org.cactoos.scalar.Unchecked;
import org.eolang.maven.TjForeign;
import org.eolang.maven.TjsForeign;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test cases for {@link Dealphaing}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class DealphaingTest {

    @Test
    void renamesEveryArgumentOfApplication(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "every argument of the application must take the name of its void, but it doesnt",
            new XMLDocument(DealphaingTest.placed(temp, false, "bar", "baz")),
            XhtmlMatchers.hasXPaths("/object/o/o[@as='bar']", "/object/o/o[@as='baz']")
        );
    }

    @Test
    void keepsEveryAlphaOfHalfNamedApplication(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an application with one argument unnamed must keep every αN, but it doesnt",
            new XMLDocument(DealphaingTest.placed(temp, false, "bar", "")),
            XhtmlMatchers.hasXPaths("/object/o/o[@as='α0']", "/object/o/o[@as='α1']")
        );
    }

    @Test
    void countsEveryLostArgumentOfApplication(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the log must count every argument of the application that found no void, but it doesnt",
            DealphaingTest.logged(temp),
            Matchers.hasItem(
                "Named 0 of 2 positional argument(s) in 1 XMIR(s), 0 are messages of ⊥, 2 found no void to be named after"
            )
        );
    }

    @Test
    void failsOnEveryLostArgumentWhenAsked(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "the failure must count every argument of the application, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> DealphaingTest.placed(temp, true, "bar", ""),
                "a half named application must stop the build when asked to, but it didnt"
            ).getMessage(),
            Matchers.containsString("2 positional argument(s)")
        );
    }

    @Test
    void keepsAlphaOfBottomMessage(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the message of ⊥ must keep its αN without failing the build, but it doesnt",
            new XMLDocument(
                DealphaingTest.dealphaed(
                    temp, true, DealphaingTest.bottom(), LandingsTest.grounded("Φ.k.φ.α0")
                )
            ),
            XhtmlMatchers.hasXPath("/object/o[@base='⊥']/o[@as='α0']")
        );
    }

    @Test
    void namesArgumentsInsideBottomMessage(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the arguments of a call inside the message of ⊥ must be named, but they arent",
            new XMLDocument(
                DealphaingTest.dealphaed(
                    temp,
                    true,
                    "<object><o base='⊥' loc='Φ.k.φ'><o base='.printf' loc='Φ.k.φ.α0' as='α0'><o/><o loc='Φ.k.φ.α0.α1' as='α1'/></o></o></object>",
                    LandingsTest.grounded("Φ.k.φ.α0", "Φ.k.φ.α0.α1"),
                    "<o loc='Φ.k.φ.α0.α1' copies='1' lands='args'><data/></o>"
                )
            ),
            XhtmlMatchers.hasXPaths(
                "/object/o[@base='⊥']/o[@as='α0']",
                "/object/o/o[@base='.printf']/o[@as='args']"
            )
        );
    }

    @Test
    void countsBottomMessageApart(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the log must count the message of ⊥ apart from the lost ones, but it doesnt",
            DealphaingTest.logged(
                () -> DealphaingTest.dealphaed(
                    temp, true, DealphaingTest.bottom(), LandingsTest.grounded("Φ.k.φ.α0")
                )
            ),
            Matchers.hasItem(
                "Named 0 of 1 positional argument(s) in 1 XMIR(s), 1 are messages of ⊥, 0 found no void to be named after"
            )
        );
    }

    private static String bottom() {
        return "<object><o base='⊥' loc='Φ.k.φ'><o loc='Φ.k.φ.α0' as='α0'/></o></object>";
    }

    private static List<String> logged(final Path temp) {
        return DealphaingTest.logged(() -> DealphaingTest.placed(temp, false, "bar", ""));
    }

    private static List<String> logged(final Scalar<Path> run) {
        final List<String> messages = new CopyOnWriteArrayList<>();
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
        final Logger logger = Logger.getLogger(Dealphaing.class);
        final Level level = logger.getLevel();
        logger.setLevel(Level.INFO);
        logger.addAppender(appender);
        try {
            new Unchecked<>(run).value();
        } finally {
            logger.removeAppender(appender);
            logger.setLevel(level);
        }
        return messages;
    }

    private static Path placed(
        final Path temp, final boolean strict, final String... lands
    ) throws IOException {
        final String call = "Φ.k.φ";
        return DealphaingTest.dealphaed(
            temp,
            strict,
            String.format(
                "<object><o loc='%1$s'><o loc='%1$s.α0' as='α0'/><o loc='%1$s.α1' as='α1'/></o></object>",
                call
            ),
            LandingsTest.grounded(call, "Φ.k.φ.α0", "Φ.k.φ.α1"),
            IntStream.range(0, lands.length).mapToObj(
                idx -> DealphaingTest.row(call, idx, lands[idx])
            ).toArray(String[]::new)
        );
    }

    private static Path dealphaed(
        final Path temp, final boolean strict, final String program,
        final String facts, final String... rows
    ) throws IOException {
        final Path xmir = temp.resolve("app.xmir");
        Files.write(xmir, program.getBytes(StandardCharsets.UTF_8));
        try (TjsForeign tojos = new TjsForeign()) {
            final TjForeign tojo = tojos.add("app").withXmir(xmir);
            new Dealphaing(
                List.of(tojo),
                LandingsTest.tables(temp, facts, rows),
                temp.resolve("target"),
                strict
            ).exec();
            return tojo.xmir();
        }
    }

    private static String row(final String call, final int idx, final String lands) {
        final String named;
        if (lands.isEmpty()) {
            named = "";
        } else {
            named = String.format(" lands='%s'", lands);
        }
        return String.format(
            "<o loc='%s.α%d' copies='1'%s><data/></o>", call, idx, named
        );
    }
}
