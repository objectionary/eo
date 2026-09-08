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
import java.nio.file.Paths;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eclipse.jetty.proxy.ProxyHandler;
import org.eclipse.jetty.server.Handler;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.ServerConnector;
import org.eclipse.jetty.server.handler.ConnectHandler;
import org.eclipse.jetty.server.handler.ResourceHandler;
import org.eclipse.jetty.util.resource.ResourceFactory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * This tests checks how eo-maven-plugin works when a proxy is set.
 *
 * <p>The sandbox is walled off from the network: the only repository it is
 * given is a Jetty serving the local {@code ~/.m2/repository}, so every
 * artifact it asks for has to be in there already. It is taken no further
 * than {@code process-sources}, the last phase the four goals under test are
 * bound to, because the phases after it bind plugins of their own, at the
 * versions Maven defaults to rather than the ones this build pins, and the
 * one missing from the local repository fails the sandbox before the proxy
 * is ever exercised.</p>
 *
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
    void checksThatWeCanCompileTheProgramWithProxySet(@Mktmp final Path tmp) throws Exception {
        final int port = ProxyIT.free();
        final Server proxy = new Server(port);
        proxy.setHandler(ProxyIT.handler());
        proxy.start();
        final Server repo = ProxyIT.repository();
        final String[] log = {""};
        try {
            new Farea(tmp).together(
                f -> {
                    ProxyIT.setupForProxy(f, port, ProxyIT.port(repo));
                    f.exec("process-sources");
                    log[0] = f.log().content();
                }
            );
        } finally {
            ProxyIT.shutdown(proxy);
            ProxyIT.shutdown(repo);
        }
        MatcherAssert.assertThat(
            "We expect the build is successful when a proxy is set",
            log[0],
            Matchers.containsString("BUILD SUCCESS")
        );
    }

    private static Handler handler() {
        final ConnectHandler connect = new ConnectHandler();
        connect.setHandler(new ProxyHandler.Forward());
        return connect;
    }

    private static Server repository() throws Exception {
        final Server server = new Server();
        final ServerConnector connector = new ServerConnector(server);
        connector.setHost("localhost");
        connector.setPort(0);
        server.addConnector(connector);
        final ResourceHandler resources = new ResourceHandler();
        resources.setDirAllowed(false);
        resources.setBaseResource(
            ResourceFactory.of(server).newResource(ProxyIT.localRepository())
        );
        server.setHandler(resources);
        server.start();
        return server;
    }

    private static Path localRepository() {
        return Paths.get(System.getProperty("user.home"), ".m2", "repository");
    }

    private static int port(final Server server) {
        return ((ServerConnector) server.getConnectors()[0]).getLocalPort();
    }

    private static void shutdown(final Server server) throws Exception {
        if (server != null && server.isStarted()) {
            server.setStopTimeout(5000L);
            server.stop();
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

    private static void setupForProxy(
        final Farea farea, final int proxy, final int repo
    ) throws IOException {
        farea.clean();
        farea.files()
            .file("src/main/eo/foo/x/y/main.eo")
            .write(ProxyIT.program().getBytes(StandardCharsets.UTF_8));
        new AppendedPlugin(farea).value()
            .goals("register", "assemble", "resolve", "place");
        farea.withOpt("-s");
        farea.withOpt(
            farea.files().file("settings.xml").write(
                ProxyIT.settings(proxy, repo).getBytes(StandardCharsets.UTF_8)
            ).path().toString()
        );
    }

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

    private static String settings(final int proxy, final int repo) {
        return new UncheckedText(new TextOf(new ResourceOf("proxy-settings.xml")))
            .asString()
            .replace("${proxy.port}", Integer.toString(proxy))
            .replace("${repo.port}", Integer.toString(repo));
    }
}
