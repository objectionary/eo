/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.Xsline;
import java.util.ArrayList;
import java.util.List;
import org.apache.log4j.Appender;
import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.LoggingEvent;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@code restore-local-names.xsl}.
 * @since 0.35.0
 */
final class RestoreLocalNamesTest {

    /**
     * A "Φ.dataized" wrapped value carrying "@local" but no "@name" - the
     * shape of a const `>>` handle's value inside the "const-to-dataized"
     * shell (#5941).
     */
    private static final String FIXTURE =
        "<object><o base='.as-bytes'><o base='Q.dataized'><o base='xi.rho.b' local='handle'/></o></o></object>";

    @Test
    void doesNotWarnOnLocalWithoutName() {
        final List<String> warnings = new ArrayList<>(0);
        final Appender appender = new AppenderSkeleton() {
            @Override
            protected void append(final LoggingEvent event) {
                if (event.getLevel().isGreaterOrEqual(Level.WARN)) {
                    warnings.add(String.valueOf(event.getRenderedMessage()));
                }
            }

            @Override
            public void close() {
                // Nothing to release: this appender holds no resources.
            }

            @Override
            public boolean requiresLayout() {
                return false;
            }
        };
        final Logger root = Logger.getRootLogger();
        root.addAppender(appender);
        try {
            new Xsline(
                new StClasspath("/org/eolang/printer/print/restore-local-names.xsl")
            ).pass(new XMLDocument(RestoreLocalNamesTest.FIXTURE));
        } finally {
            root.removeAppender(appender);
        }
        MatcherAssert.assertThat(
            "restore-local-names.xsl must not warn about an empty sequence when a @local node has no @name",
            warnings.stream().noneMatch(msg -> msg.contains("empty sequence")),
            Matchers.is(true)
        );
    }
}
