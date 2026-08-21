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
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Proxies}.
 * @since 0.62.0
 */
final class ProxiesTest {

    @Test
    void translatesActiveProxy() {
        MatcherAssert.assertThat(
            "Active proxy of settings must keep its address",
            new Proxies(ProxiesTest.settings("http", "prox.eolang.org", 8431, true)).value()[0]
                .address(),
            Matchers.equalTo(
                new Proxy(
                    Proxy.Type.HTTP,
                    InetSocketAddress.createUnresolved("prox.eolang.org", 8431)
                )
            )
        );
    }

    @Test
    void translatesActiveSocksProxyToItsOwnType() {
        MatcherAssert.assertThat(
            "A socks5 proxy of settings must become a SOCKS java.net.Proxy, not HTTP",
            new Proxies(ProxiesTest.settings("socks5", "socks.eolang.org", 1080, true))
                .value()[0].address().type(),
            Matchers.equalTo(Proxy.Type.SOCKS)
        );
    }

    @Test
    void skipsInactiveProxy() {
        MatcherAssert.assertThat(
            "Inactive proxy of settings must be dropped",
            new Proxies(ProxiesTest.settings("http", "dead.eolang.org", 3129, false)).value(),
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

    private static Settings settings(
        final String protocol, final String host, final int port, final boolean active
    ) {
        final org.apache.maven.settings.Proxy proxy = new org.apache.maven.settings.Proxy();
        proxy.setProtocol(protocol);
        proxy.setHost(host);
        proxy.setPort(port);
        proxy.setActive(active);
        final Settings settings = new Settings();
        settings.addProxy(proxy);
        return settings;
    }
}
