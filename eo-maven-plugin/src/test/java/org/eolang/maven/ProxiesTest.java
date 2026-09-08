/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.net.InetSocketAddress;
import java.net.Proxy;
import org.apache.maven.settings.Settings;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Proxies}.
 *
 * @since 0.62.0
 */
final class ProxiesTest {

    @Test
    void translatesActiveProxy() {
        MatcherAssert.assertThat(
            "Active proxy of settings must keep its address",
            ProxiesTest.only(ProxiesTest.settings("prox.eolang.org", 8431, true)).address(),
            Matchers.equalTo(
                new Proxy(
                    Proxy.Type.HTTP,
                    InetSocketAddress.createUnresolved("prox.eolang.org", 8431)
                )
            )
        );
    }

    @Test
    void skipsInactiveProxy() {
        MatcherAssert.assertThat(
            "Inactive proxy of settings must be dropped",
            new Proxies(ProxiesTest.settings("dead.eolang.org", 3129, false)).value(),
            Matchers.emptyArray()
        );
    }

    @Test
    void staysEmptyWithoutProxies() {
        MatcherAssert.assertThat(
            "Settings without proxies must give no proxies at all",
            new Proxies(new Settings()).value(),
            Matchers.emptyArray()
        );
    }

    @Test
    void refusesAProxyThatSpeaksSomethingElse() {
        final Settings settings = ProxiesTest.settings("socks.eolang.org", 1080, true);
        settings.getProxies().get(0).setProtocol("socks5");
        MatcherAssert.assertThat(
            "the protocol must be named, since only an HTTP proxy can be used here",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Proxies(settings).value(),
                "a proxy nothing can talk to must not be taken for one that works"
            ).getMessage(),
            Matchers.containsString("socks5")
        );
    }

    private static MvnProxy only(final Settings settings) {
        return new Proxies(settings).value()[0];
    }

    private static Settings settings(final String host, final int port, final boolean active) {
        final org.apache.maven.settings.Proxy proxy = new org.apache.maven.settings.Proxy();
        proxy.setHost(host);
        proxy.setPort(port);
        proxy.setActive(active);
        final Settings settings = new Settings();
        settings.addProxy(proxy);
        return settings;
    }
}
