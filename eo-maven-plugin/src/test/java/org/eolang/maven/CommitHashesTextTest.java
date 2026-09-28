/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Together;
import com.yegor256.WeAreOnline;
import javax.net.ssl.SSLException;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link CommitHashesText}.
 *
 * @since 0.37.0
 */
final class CommitHashesTextTest {

    @Test
    void fallsBackOnATransportFailureThatIsNotUnknownHost() throws Exception {
        MatcherAssert.assertThat(
            "A transport failure other than an unknown host must still fall back to the built-in table, not propagate and fail the build",
            new CommitHashesText(
                () -> {
                    throw new SSLException("handshake failed");
                }
            ).asString(),
            Matchers.containsString("0.28.9")
        );
    }

    @Test
    void findsEveryTagOfTheFallbackTable() {
        MatcherAssert.assertThat(
            "ChText must find a tag in the built-in fallback table, whatever the platform's line separator is",
            new ChText(
                new CommitHashesText(
                    () -> {
                        throw new SSLException("handshake failed");
                    }
                )::asString,
                "0.23.15"
            ).value(),
            Matchers.equalTo("5fe5ad8d21dbe418038fa4c86e096fb037f290a9")
        );
    }

    @Test
    void propagatesAFailureThatIsNotAnIoError() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new CommitHashesText(
                () -> {
                    throw new IllegalStateException("not an I/O failure");
                }
            ).asString(),
            "A non-I/O failure must not be swallowed into the fallback table"
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void downloadsDefaultList() throws Exception {
        MatcherAssert.assertThat(
            "CommitHashesText downloads the default list of hashes from Objectionary",
            new CommitHashesText().asString(),
            Matchers.containsString("master")
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void isThreadSafe() {
        final CommitHashesText text = new CommitHashesText();
        MatcherAssert.assertThat(
            "Can be used in different threads without NPE",
            new Together<>(
                thread -> text.asString() != null
            ),
            Matchers.not(Matchers.hasItems(false))
        );
    }
}
