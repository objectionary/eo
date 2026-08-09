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
            new Proxies(ProxiesTest.settings("prox.eolang.org", 8431, true)).value(),
            Matchers.arrayContaining(
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

    /**
     * Maven settings with a single proxy in them.
     * @param host Host of the proxy
     * @param port Port of the proxy
     * @param active Is the proxy active?
     * @return Settings with the proxy
     */
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
