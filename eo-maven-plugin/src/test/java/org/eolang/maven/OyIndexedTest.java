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
import java.util.Collections;
import org.cactoos.io.InputOf;
import org.cactoos.set.SetOf;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link OyIndexed}.
 *
 * @since 0.29
 */
final class OyIndexedTest {

    @Test
    void getsFromDelegate() throws Exception {
        MatcherAssert.assertThat(
            "OyIndexed must get a line of program, but it doesn't",
            new TextOf(new OyIndexed(new Objectionary.Fake()).get("foo")).asString(),
            Matchers.equalTo(
                String.join(
                    System.lineSeparator(),
                    "[] > sprintf",
                    ""
                )
            )
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void containsInRealIndex() throws IOException {
        MatcherAssert.assertThat(
            "OyIndexed must contain stdout object, but it doesn't",
            new OyIndexed(new Objectionary.Fake()).contains(this.stdout()),
            Matchers.is(true)
        );
    }

    @Test
    void containsInFakeIndex() throws IOException {
        MatcherAssert.assertThat(
            "OyIndexed with fake index must contain stdout object, but it doesn't",
            new OyIndexed(
                new Objectionary.Fake(),
                new ObjectsIndex(() -> Collections.singleton("stdout"))
            ).contains(this.stdout()),
            Matchers.is(true)
        );
    }

    @Test
    void checksContainsInDelegateIfExceptionHappensInIndex() throws IOException {
        MatcherAssert.assertThat(
            "OyIndexed with an exception must contain stdout object, but it doesn't",
            new OyIndexed(
                new Objectionary.Fake(),
                new ObjectsIndex(
                    () -> {
                        throw new IllegalStateException("Fake exception");
                    }
                )
            ).contains(this.stdout()),
            Matchers.is(true)
        );
    }

    @Test
    void checksIsDirectoryInDelegateIfExceptionHappensInIndex() throws IOException {
        MatcherAssert.assertThat(
            "OyIndexed with a broken index must ask the delegate about a directory, but it doesnt",
            new OyIndexed(
                new Objectionary.Fake(
                    name -> new InputOf("[] > qwerty"),
                    name -> false,
                    name -> true
                ),
                new ObjectsIndex(
                    () -> {
                        throw new IllegalStateException("Fake exception");
                    }
                )
            ).isDirectory("org.eolang.qwerty"),
            Matchers.is(true)
        );
    }

    @Test
    void keepsDisambiguatingObjectFromPackageInDelegateFallback() throws IOException {
        MatcherAssert.assertThat(
            "OyIndexed with a broken index must not call a name a directory when the delegate also has it as a program",
            new OyIndexed(
                new Objectionary.Fake(
                    name -> new InputOf("[] > tuple"),
                    name -> true,
                    name -> true
                ),
                new ObjectsIndex(
                    () -> {
                        throw new IllegalStateException("Fake exception");
                    }
                )
            ).isDirectory("org.eolang.tuple"),
            Matchers.is(false)
        );
    }

    @Test
    void keepsDirectoryOnlyNameADirectoryWithoutIndex() throws Exception {
        final HttpServer server = HttpServer.create(
            new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0
        );
        server.createContext(
            "/",
            exchange -> {
                final int status;
                if (exchange.getRequestURI().getPath().endsWith(".eo")) {
                    status = 404;
                } else {
                    status = 200;
                }
                exchange.sendResponseHeaders(status, -1L);
                exchange.close();
            }
        );
        server.start();
        try {
            final String base = String.format(
                "http://127.0.0.1:%d", server.getAddress().getPort()
            );
            MatcherAssert.assertThat(
                "a name the remote has only as a directory must stay a directory while the index cannot be read, but it is taken for a program",
                new OyIndexed(
                    new OyCached(
                        new OyRemote(
                            new UrlOy(String.format("%s/objects/%%s/%%s.eo", base), "rev"),
                            new UrlOy(String.format("%s/tree/%%s/%%s", base), "rev")
                        )
                    ),
                    new ObjectsIndex(
                        () -> {
                            throw new IOException("the index is not there");
                        }
                    )
                ).isDirectory("org.eolang.example"),
                Matchers.is(true)
            );
        } finally {
            server.stop(0);
        }
    }

    @Test
    void listsChildrenFromFakeIndex() throws IOException {
        MatcherAssert.assertThat(
            "OyIndexed with fake index must list the children of the package, but it doesn't",
            new OyIndexed(
                new Objectionary.Fake(),
                new ObjectsIndex(() -> new SetOf<>("tuple.each", "tuple.eachi"))
            ).children("tuple"),
            Matchers.containsInAnyOrder("tuple.each", "tuple.eachi")
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void checksIsDirectoryForObject() throws IOException {
        MatcherAssert.assertThat(
            "OyIndexed must contain stdout object, but it doesn't",
            new OyIndexed(new Objectionary.Fake()).isDirectory(this.stdout()),
            Matchers.is(false)
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void checksIsDirectoryForDirectory() throws IOException {
        MatcherAssert.assertThat(
            "OyIndexed must not contain directory, but it does",
            new OyIndexed(new Objectionary.Fake()).isDirectory("xxx"),
            Matchers.is(false)
        );
    }

    private String stdout() {
        return "stdout";
    }
}
