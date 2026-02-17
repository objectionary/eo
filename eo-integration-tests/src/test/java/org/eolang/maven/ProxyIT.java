/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.MayBeSlow;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ProxySelector;
import java.net.ServerSocket;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eclipse.jetty.http.HttpStatus;
import org.eclipse.jetty.proxy.ProxyHandler;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.handler.ConnectHandler;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * This tests checks how eo-maven-plugin works when a proxy is set.
 * @since 0.60
 */
@SuppressWarnings({
    "JTCOP.RuleAllTestsHaveProductionClass",
    "PMD.UnitTestShouldIncludeAssert",
    "PMD.UnitTestContainsTooManyAsserts",
    "PMD.UnnecessaryLocalRule",
    "PMD.UseExplicitTypes"
})
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class ProxyIT {

    /**
     * The proxy server.
     */
    private Server proxy;

    /**
     * The proxy port.
     */
    private int port;

    @BeforeEach
    void setupProxy() throws Exception {
        this.port = ProxyIT.free();
        this.proxy = new Server(this.port);
        this.proxy.setHandler(new ProxyHandler.Forward());
        this.proxy.setHandler(new ConnectHandler());
        this.proxy.start();
    }

    @Test
    void checksThatProxyIsWorking() throws IOException, InterruptedException {
        final HttpResponse<String> resp = HttpClient.newBuilder()
            .proxy(ProxySelector.of(new InetSocketAddress("localhost", this.port)))
            .followRedirects(HttpClient.Redirect.NORMAL)
            .build()
            .send(
                HttpRequest.newBuilder()
                    .uri(URI.create("https://objectionary.com/"))
                    .header("User-Agent", "test-client")
                    .GET()
                    .build(), HttpResponse.BodyHandlers.ofString()
            );
        MatcherAssert.assertThat(
            "Proxy should return 200 OK for objectionary.com",
            resp.statusCode(),
            Matchers.equalTo(HttpStatus.OK_200)
        );
        MatcherAssert.assertThat(
            "Response body should contain objectionary.com",
            resp.body(),
            Matchers.allOf(
                Matchers.containsString("objectionary"),
                Matchers.containsString("sources")
            )
        );
    }

    @Test
    void checksThatWeCanCompileTheProgramWithProxySet(@Mktmp final Path tmp) throws Exception {
        new Farea(tmp).together(
            f -> {
                f.clean();
                final var settings = f.files().file("settings.xml").write(
                    ProxyIT.settings(this.port).getBytes(StandardCharsets.UTF_8)
                );
                f.files()
                    .file("src/main/eo/foo/x/y/main.eo")
                    .write(ProxyIT.program().getBytes(StandardCharsets.UTF_8));
                new AppendedPlugin(f).value()
                    .goals("register", "assemble", "resolve", "place");
                f.withOpt("-s");
                f.withOpt(settings.path().toString());
                f.exec("package");
                MatcherAssert.assertThat(
                    "We expect the build is successful when a proxy is set",
                    f.log().content(),
                    Matchers.containsString("BUILD SUCCESS")
                );
            }
        );
    }

    @AfterEach
    void stopProxy() throws Exception {
        if (this.proxy != null && this.proxy.isStarted()) {
            this.proxy.setStopTimeout(5000);
            this.proxy.setStopAtShutdown(true);
            try {
                this.proxy.stop();
            } finally {
                this.proxy.destroy();
            }
        }
    }

    /**
     * Finds a free port.
     * @return Free port number
     */
    private static int free() {
        try (ServerSocket socket = new ServerSocket(0)) {
            return socket.getLocalPort();
        } catch (final IOException exception) {
            throw new IllegalStateException("Could not find a free port", exception);
        }
    }

    private static String program() {
        return String.join(
            "\n",
            "+alias stdout org.eolang.io.stdout",
            "+package foo.x.y",
            "+version 0.1.2",
            "",
            "# Prints Hello Proxy! to stdout.",
            "[x] > main",
            "  (stdout \"Hello Proxy!\" x).print > @"
        );
    }

    /**
     * Returns proxy settings XML with the given port.
     * @param port Proxy port.
     * @return Proxy settings XML.
     */
    private static String settings(final int port) {
        return new UncheckedText(new TextOf(new ResourceOf("proxy-settings.xml")))
            .asString()
            .replace("${proxy.port}", Integer.toString(port));
    }
}
