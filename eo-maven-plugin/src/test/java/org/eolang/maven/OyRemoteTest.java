/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.WeAreOnline;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link OyRemote}.
 *
 * @since 0.1.0
 */
final class OyRemoteTest {

    @Test
    void buildsCorrectUrl() throws Exception {
        MatcherAssert.assertThat(
            "OyRemove.UrlOy generates correct URL",
            new OyRemote.UrlOy(
                "https://raw/objectionary/home/%s/objects/%s.eo",
                "abcde"
            ).value("org.eolang.app"),
            Matchers.is(
                new URL("https://raw/objectionary/home/abcde/objects/org/eolang/app.eo")
            )
        );
    }

    @Test
    void throwsExceptionOnInvalidUrl() {
        Assertions.assertThrows(
            MalformedURLException.class,
            () -> new OyRemote.UrlOy(
                "hts:raw.githubusercontent.com/objectionary/home/%s/objects/%s.eo",
                "abcde"
            ).value("org.eolang.app"),
            CatalogsTest.TO_ADD_MESSAGE
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void checksPresenceOfObject() throws IOException {
        final CommitHash hash = new ChRemote("master");
        final Objectionary objectionary = new OyRemote(hash);
        MatcherAssert.assertThat(
            "OyRemote positively checks the presence of the object in Objectionary",
            objectionary.contains("org.eolang.io.stdout"),
            Matchers.is(true)
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void checksPresenceOfObjectWithNarrowHash() throws IOException {
        final String stdout = "org.eolang.io.stdout";
        MatcherAssert.assertThat(
            String.format(
                "OyRemote with narrow hash should have contained object %s, but it didn't",
                stdout
            ),
            new OyRemote(
                new ChNarrow(
                    new ChRemote("master")
                )
            ).contains(stdout),
            Matchers.is(true)
        );
    }
}
