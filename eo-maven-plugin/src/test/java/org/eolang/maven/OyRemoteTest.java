/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.sun.net.httpserver.HttpServer;
import com.yegor256.WeAreOnline;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
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
    void buildsCorrectUrlForProgram() throws Exception {
        MatcherAssert.assertThat(
            "UrlOy generates correct URL for program",
            new UrlOy(
                "https://raw/objectionary/home/%s/objects/%s.eo",
                "abcde"
            ).value("org.eolang.io.stdout"),
            Matchers.is(
                new URL("https://raw/objectionary/home/abcde/objects/io/stdout.eo")
            )
        );
    }

    @Test
    void buildsCorrectUrlForDirectory() throws Exception {
        MatcherAssert.assertThat(
            "UrlOy generates correct URL for directory",
            new UrlOy(
                "https://github.com/objectionary/home/tree/%s/objects/%s",
                "abcde"
            ).value("org.eolang.ss"),
            Matchers.is(
                new URL("https://github.com/objectionary/home/tree/abcde/objects/ss")
            )
        );
    }

    @Test
    void throwsExceptionOnInvalidUrlForProgram() {
        Assertions.assertThrows(
            MalformedURLException.class,
            () -> new UrlOy(
                "hts:raw.githubusercontent.com/objectionary/home/%s/objects/%s.eo",
                "xyz"
            ).value("org.eolang.app"),
            "Expected MalformedURLException when the URL format is invalid"
        );
    }

    @Test
    void throwsExceptionOnInvalidUrlForDirectory() {
        Assertions.assertThrows(
            MalformedURLException.class,
            () -> new UrlOy(
                "hts:github.com/objectionary/home/tree/%s/objects/%s",
                "xyz"
            ).value("org.eolang.dir"),
            "Expected MalformedURLException when the URL format is invalid"
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void checksPresenceOfProgram() throws IOException {
        MatcherAssert.assertThat(
            "OyRemote positively checks the presence of the program in Objectionary",
            new OyRemote(new ChRemote("master")).contains("stdout"),
            Matchers.is(true)
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void checksPresenceOfDirectory() throws IOException {
        MatcherAssert.assertThat(
            "OyRemote positively checks the presence of the directory in Objectionary",
            new OyRemote(new ChRemote("master")).isDirectory("number"),
            Matchers.is(true)
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void checksPresenceOfProgramWithNarrowHash() throws IOException {
        final String stdout = "stdout";
        MatcherAssert.assertThat(
            String.format(
                "OyRemote with narrow hash should have contained program %s, but it didn't",
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

    @Test
    void doesNotReturnThrottledResponseBodyAsSource() throws Exception {
        final HttpServer server = HttpServer.create(
            new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0
        );
        server.createContext(
            "/",
            exchange -> {
                final byte[] body = "429: Too Many Requests".getBytes(StandardCharsets.UTF_8);
                exchange.sendResponseHeaders(429, body.length);
                exchange.getResponseBody().write(body);
                exchange.close();
            }
        );
        server.start();
        try {
            final String tpl = String.format(
                "http://127.0.0.1:%d/%%s/%%s.eo", server.getAddress().getPort()
            );
            Assertions.assertThrows(
                IOException.class,
                () -> new OyRemote(
                    new UrlOy(tpl, "stub"),
                    new UrlOy(tpl, "stub")
                ).get("org.eolang.foo").stream(),
                "Expected an IOException instead of the throttled HTTP body being returned as EO source"
            );
        } finally {
            server.stop(0);
        }
    }

    @Test
    void reportsNameInTheEoObjectSlotWhenObjectNotFound() throws Exception {
        MatcherAssert.assertThat(
            "The object name must show up in the 'EO object' slot of the message",
            OyRemoteTest.notFoundMessage(),
            Matchers.containsString("EO object 'org.eolang.txt.sprintf'")
        );
    }

    @Test
    void reportsUrlInTheByUrlSlotWhenObjectNotFound() throws Exception {
        MatcherAssert.assertThat(
            "The URL must show up in the 'by url' slot of the message",
            OyRemoteTest.notFoundMessage(),
            Matchers.containsString("by url: http://127.0.0.1")
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void checksPresenceOfDirectoryWithNarrowHash() throws IOException {
        final String directory = "tuple";
        MatcherAssert.assertThat(
            String.format(
                "OyRemote with narrow hash should have contained directory %s, but it didn't",
                directory
            ),
            new OyRemote(
                new ChNarrow(
                    new ChRemote("master")
                )
            ).isDirectory(directory),
            Matchers.is(true)
        );
    }

    private static String notFoundMessage() throws Exception {
        final HttpServer server = HttpServer.create(
            new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0
        );
        server.createContext(
            "/",
            exchange -> {
                final byte[] body = "404: Not Found".getBytes(StandardCharsets.UTF_8);
                exchange.sendResponseHeaders(404, body.length);
                exchange.getResponseBody().write(body);
                exchange.close();
            }
        );
        server.start();
        try {
            final String tpl = String.format(
                "http://127.0.0.1:%d/%%s/%%s.eo", server.getAddress().getPort()
            );
            return Assertions.assertThrows(
                IOException.class,
                () -> new OyRemote(
                    new UrlOy(tpl, "stub"),
                    new UrlOy(tpl, "stub")
                ).get("org.eolang.txt.sprintf").stream(),
                "Expected an IOException reporting the object as not found"
            ).getMessage();
        } finally {
            server.stop(0);
        }
    }
}
