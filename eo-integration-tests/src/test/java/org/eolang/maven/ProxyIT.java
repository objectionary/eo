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
import org.eclipse.jetty.proxy.ProxyHandler;
import org.eclipse.jetty.server.Handler;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.handler.ConnectHandler;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * This tests checks how eo-maven-plugin works when a proxy is set.
 * @since 0.60
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class ProxyIT {

    @Test
    void checksThatProxyIsWorking() throws Exception {
        final int port = ProxyIT.free();
        final Server proxy = new Server(port);
        proxy.setHandler(ProxyIT.handler());
        proxy.start();
        try {
            MatcherAssert.assertThat(
                "Response body should contain objectionary.com",
                ProxyIT.fetchThroughProxy(port),
                Matchers.allOf(
                    Matchers.containsString("objectionary"),
                    Matchers.containsString("sources")
                )
            );
        } finally {
            ProxyIT.shutdown(proxy);
        }
    }

    @Test
    @Disabled("still depends on live Maven Central through the proxy, see #5429")
    void checksThatWeCanCompileTheProgramWithProxySet(@Mktmp final Path tmp) throws Exception {
        final int port = ProxyIT.free();
        final Server proxy = new Server(port);
        proxy.setHandler(ProxyIT.handler());
        proxy.start();
        final String[] log = {""};
        try {
            new Farea(tmp).together(
                f -> {
                    ProxyIT.setupForProxy(f, port);
                    f.exec("package");
                    log[0] = f.log().content();
                }
            );
        } finally {
            ProxyIT.shutdown(proxy);
        }
        MatcherAssert.assertThat(
            "We expect the build is successful when a proxy is set",
            log[0],
            Matchers.containsString("BUILD SUCCESS")
        );
    }

    /**
     * Build a forward proxy handler that also tunnels CONNECT requests.
     *
     * <p>{@link ConnectHandler} is a handler wrapper: it serves {@code CONNECT}
     * requests itself (HTTPS tunneling) and delegates every other request to
     * the handler nested inside it. Nesting {@link ProxyHandler.Forward} into
     * it therefore installs both behaviours at once. Calling
     * {@code setHandler(...)} twice on the server, as it used to be done here,
     * simply replaced the first handler with the second, so plain HTTP
     * forwarding was never exercised (see #5110 and #5429).</p>
     *
     * @return The combined handler
     */
    private static Handler handler() {
        final ConnectHandler connect = new ConnectHandler();
        connect.setHandler(new ProxyHandler.Forward());
        return connect;
    }

    private static void shutdown(final Server proxy) throws Exception {
        if (proxy != null && proxy.isStarted()) {
            proxy.setStopTimeout(5000);
            proxy.setStopAtShutdown(true);
            try {
                proxy.stop();
            } finally {
                proxy.destroy();
            }
        }
    }

    private static String fetchThroughProxy(final int port)
        throws IOException, InterruptedException {
        return HttpClient.newBuilder()
            .proxy(ProxySelector.of(new InetSocketAddress("localhost", port)))
            .followRedirects(HttpClient.Redirect.NORMAL)
            .build().send(
                HttpRequest.newBuilder()
                    .uri(URI.create("https://objectionary.com/"))
                    .header("User-Agent", "test-client")
                    .GET()
                    .build(),
                HttpResponse.BodyHandlers.ofString()
            ).body();
    }

    private static void setupForProxy(final Farea farea, final int port) throws IOException {
        farea.clean();
        farea.files()
            .file("src/main/eo/foo/x/y/main.eo")
            .write(ProxyIT.program().getBytes(StandardCharsets.UTF_8));
        new AppendedPlugin(farea).value()
            .goals("register", "assemble", "resolve", "place");
        farea.withOpt("-s");
        farea.withOpt(
            farea.files().file("settings.xml").write(
                ProxyIT.settings(port).getBytes(StandardCharsets.UTF_8)
            ).path().toString()
        );
    }

    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    private static int free() {
        try (ServerSocket socket = new ServerSocket(0)) {
            return socket.getLocalPort();
        } catch (final IOException exception) {
            throw new IllegalStateException("Could not find a free port", exception);
        }
    }

    private static String program() {
        return String.join(
            System.lineSeparator(),
            "+alias stdout io.stdout",
            "+package foo.x.y",
            "+version 0.1.2",
            "",
            "[x] > main",
            "  (stdout \"Hello Proxy!\" x).print > @"
        );
    }

    /**
     * Returns proxy settings XML with the given port.
     * @param port Proxy port
     * @return Proxy settings XML
     */
    private static String settings(final int port) {
        return new UncheckedText(new TextOf(new ResourceOf("proxy-settings.xml")))
            .asString()
            .replace("${proxy.port}", Integer.toString(port));
    }
}
