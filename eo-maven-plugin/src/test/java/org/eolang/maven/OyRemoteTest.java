/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.sun.net.httpserver.HttpServer;
import com.yegor256.WeAreOnline;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Arrays;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.function.ThrowingSupplier;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test for {@link OyRemote}.
 *
 * @since 0.1.0
 */
final class OyRemoteTest {

    @ParameterizedTest
    @CsvSource({
        "0,false", "1,false", "2,false", "0,true", "1,true", "2,true"
    })
    void boundsRequestsWaitingForHeaders(final int operation, final boolean responds)
        throws Exception {
        final CountDownLatch arrived = new CountDownLatch(3);
        final HttpServer server = HttpServer.create(
            new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0
        );
        server.setExecutor(
            command -> {
                final Thread thread = new Thread(command);
                thread.setDaemon(true);
                thread.start();
            }
        );
        server.createContext(
            "/", exchange -> {
                arrived.countDown();
                if (responds) {
                    final byte[] body = "[] > foo".getBytes(StandardCharsets.UTF_8);
                    exchange.sendResponseHeaders(200, body.length);
                    exchange.getResponseBody().write(body);
                    exchange.close();
                }
            }
        );
        server.start();
        try {
            final UrlOy template = new UrlOy(
                String.format("http://127.0.0.1:%d/%%s/%%s", server.getAddress().getPort()), "stub"
            );
            final OyRemote remote = new OyRemote(template, template, Duration.ofSeconds(5));
            final ThrowingSupplier<Boolean> request = () -> switch (operation) {
                case 0 -> remote.isProgram("foo");
                case 1 -> remote.isDirectory("foo");
                default -> "[] > foo".equals(new TextOf(remote.get("foo")).asString());
            };
            Assertions.assertTimeoutPreemptively(
                Duration.ofSeconds(90),
                () -> {
                    if (responds) {
                        Assertions.assertTrue(request.get(), "The immediate response must succeed");
                    } else {
                        Assertions.assertThrows(
                            IOException.class, request::get,
                            "Withheld headers must exhaust retries with an IOException"
                        );
                        MatcherAssert.assertThat(
                            "The server must receive retried requests before failure",
                            arrived.await(30L, TimeUnit.SECONDS), Matchers.is(true)
                        );
                    }
                },
                "Remote calls must finish even when the server withholds headers"
            );
        } finally {
            server.stop(0);
        }
    }

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
            OyRemoteTest.message(404),
            Matchers.containsString("EO object 'org.eolang.txt.sprintf'")
        );
    }

    @Test
    void reportsUrlInTheByUrlSlotWhenObjectNotFound() throws Exception {
        MatcherAssert.assertThat(
            "The URL must show up in the 'by url' slot of the message",
            OyRemoteTest.message(404),
            Matchers.containsString("by url: http://127.0.0.1")
        );
    }

    @Test
    void doesNotBlameTheNameWhenTheServerFails() throws Exception {
        MatcherAssert.assertThat(
            "A server error must be reported as a failed download, not as a misspelled name",
            OyRemoteTest.message(503),
            Matchers.both(
                Matchers.containsString("HTTP error 503")
            ).and(
                Matchers.not(Matchers.containsString("misspelled"))
            )
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

    @Test
    void releasesBodyOfNotFoundResponse() throws Exception {
        MatcherAssert.assertThat(
            "The body of a 404 response must be released, while it stays open",
            OyRemoteTest.released(404),
            Matchers.is(true)
        );
    }

    @Test
    void releasesBodyOfThrottledResponse() throws Exception {
        MatcherAssert.assertThat(
            "The body of a 429 response must be released, while it stays open",
            OyRemoteTest.released(429),
            Matchers.is(true)
        );
    }

    @ParameterizedTest
    @CsvSource({
        "200, false", "200, true",
        "404, false", "404, true",
        "500, false", "500, true",
        "503, false", "503, true",
        "599, false", "599, true",
        "600, false", "600, true"
    })
    void doesNotMemoizeServerErrors(final int code, final boolean directory) throws Exception {
        final AtomicInteger status = new AtomicInteger(code);
        final AtomicInteger requests = new AtomicInteger();
        final HttpServer server = HttpServer.create(
            new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0
        );
        server.createContext(
            "/", exchange -> {
                requests.incrementAndGet();
                final byte[] body = "presence".getBytes(StandardCharsets.UTF_8);
                exchange.sendResponseHeaders(status.get(), body.length);
                exchange.getResponseBody().write(body);
                exchange.close();
            }
        );
        server.start();
        try {
            final String template = String.format(
                "http://127.0.0.1:%d/%%s/%%s", server.getAddress().getPort()
            );
            final OyRemote remote = new OyRemote(
                new UrlOy(template, "stub"), new UrlOy(template, "stub")
            );
            final OyCached cached = new OyCached(remote);
            boolean refused = false;
            boolean first = false;
            try {
                first = OyRemoteTest.presence(cached, directory);
            } catch (final IOException error) {
                refused = true;
            }
            final int before = requests.get();
            status.set(200);
            final boolean recovered = OyRemoteTest.presence(cached, directory);
            final int after = requests.get();
            final boolean direct = OyRemoteTest.presence(remote, directory);
            final boolean temporary = code >= 500 && code < 600;
            MatcherAssert.assertThat(
                "Temporary server errors must not be cached as missing; ordinary checks stay cached",
                Arrays.asList(refused, first, recovered, after > before, direct),
                Matchers.equalTo(
                    Arrays.asList(
                        temporary, !temporary && code == 200,
                        temporary || code == 200, temporary, true
                    )
                )
            );
        } finally {
            server.stop(0);
        }
    }

    private static boolean released(final int status) throws Exception {
        final long flood = 64L * 1024 * 1024;
        final CountDownLatch cut = new CountDownLatch(1);
        final HttpServer server = HttpServer.create(
            new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0
        );
        server.setExecutor(
            command -> {
                final Thread thread = new Thread(command);
                thread.setDaemon(true);
                thread.start();
            }
        );
        server.createContext(
            "/",
            exchange -> {
                exchange.sendResponseHeaders(status, flood);
                try (OutputStream body = exchange.getResponseBody()) {
                    final byte[] chunk = new byte[8192];
                    for (long sent = 0L; sent < flood; sent += chunk.length) {
                        body.write(chunk);
                    }
                } catch (final IOException ex) {
                    cut.countDown();
                }
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
                "Expected an IOException for the rejected HTTP status"
            );
            return cut.await(1L, TimeUnit.MINUTES);
        } finally {
            server.stop(0);
        }
    }

    private static boolean presence(final Objectionary source, final boolean directory)
        throws IOException {
        final boolean present;
        if (directory) {
            present = source.isDirectory("foo");
        } else {
            present = source.contains("foo");
        }
        return present;
    }

    private static String message(final int status) throws Exception {
        final HttpServer server = HttpServer.create(
            new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0
        );
        server.createContext(
            "/",
            exchange -> {
                final byte[] body = "the body".getBytes(StandardCharsets.UTF_8);
                exchange.sendResponseHeaders(status, body.length);
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
                "Expected an IOException reporting the rejected HTTP status"
            ).getMessage();
        } finally {
            server.stop(0);
        }
    }
}
