/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.sun.net.httpserver.HttpServer;
import com.yegor256.WeAreOnline;
import java.io.IOException;
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
 * @since 0.1.0
 */
final class OyRemoteTest {

    @Test
    void buildsCorrectUrlForProgram() throws Exception {
        MatcherAssert.assertThat(
            "OyRemote.UrlOy generates correct URL for program",
            new OyRemote.UrlOy(
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
            "OyRemote.UrlOy generates correct URL for directory",
            new OyRemote.UrlOy(
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
            () -> new OyRemote.UrlOy(
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
            () -> new OyRemote.UrlOy(
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
        final HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
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
                    new OyRemote.UrlOy(tpl, "stub"),
                    new OyRemote.UrlOy(tpl, "stub")
                ).get("org.eolang.foo").stream(),
                "Expected an IOException instead of the throttled HTTP body being returned as EO source"
            );
        } finally {
            server.stop(0);
        }
    }

    @Test
    void putsObjectNameBeforeUrlInMissingObjectMessage() throws Exception {
        final HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext(
            "/",
            exchange -> {
                exchange.sendResponseHeaders(404, -1);
                exchange.close();
            }
        );
        server.start();
        try {
            final int port = server.getAddress().getPort();
            final String tpl = String.format(
                "http://127.0.0.1:%d/%%s/%%s.eo", port
            );
            final String name = "org.eolang.txt.sprintf";
            Assertions.assertThrows(
                IOException.class,
                () -> OyRemoteTest.missing(tpl, name, port),
                "Expected an IOException when the remote object is missing"
            );
        } finally {
            server.stop(0);
        }
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

    /**
     * Pull a missing object and verify 404 message order.
     * @param template URL template with hash and path placeholders
     * @param name Object name
     * @param port Local HTTP port used in the expected URL fragment
     * @throws Exception Always, when remote returns 404 with a correct message
     */
    private static void missing(final String template, final String name, final int port)
        throws Exception {
        try {
            new OyRemote(
                new OyRemote.UrlOy(template, "deadbeef"),
                new OyRemote.UrlOy(template, "deadbeef")
            ).get(name).stream();
        } catch (final IOException exception) {
            final String message = exception.getMessage();
            if (!message.startsWith(String.format("EO object '%s' is not found", name))
                || !message.contains(
                    String.format(
                        "by url: http://127.0.0.1:%d/deadbeef/txt/sprintf.eo.",
                        port
                    )
                )) {
                throw new AssertionError(
                    String.format(
                        "Missing-object message should put the object name first and the URL after 'by url:', but was: %s",
                        message
                    ),
                    exception
                );
            }
            throw exception;
        }
    }
}
