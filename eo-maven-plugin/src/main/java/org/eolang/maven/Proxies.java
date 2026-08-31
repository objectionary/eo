/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collection;
import java.util.stream.Collectors;
import org.apache.maven.settings.Proxy;
import org.apache.maven.settings.Settings;
import org.cactoos.Scalar;

/**
 * Active proxies of Maven settings, translated to the ones of Java.
 *
 * <p>Only an HTTP proxy is translated. {@code java.net.http} speaks to no
 * other kind, so a {@code socks5} one would be taken from the settings and
 * then quietly stepped around, which is worse than saying it cannot be
 * used.</p>
 *
 * @since 0.62.0
 */
final class Proxies implements Scalar<MvnProxy[]> {

    /**
     * Maven settings.
     */
    private final Settings settings;

    /**
     * Ctor.
     * @param settings Maven settings
     */
    Proxies(final Settings settings) {
        this.settings = settings;
    }

    @Override
    public MvnProxy[] value() {
        final Collection<Proxy> active =
            this.settings.getProxies()
                .stream()
                .filter(Proxy::isActive)
                .collect(Collectors.toList());
        for (final Proxy proxy : active) {
            final String protocol = proxy.getProtocol();
            if (protocol != null && !protocol.isEmpty()
                && !"http".equalsIgnoreCase(protocol) && !"https".equalsIgnoreCase(protocol)) {
                throw new IllegalStateException(
                    String.format(
                        "The proxy %s:%d speaks %s, and only an HTTP proxy can be used here",
                        proxy.getHost(), proxy.getPort(), protocol
                    )
                );
            }
        }
        return active.stream().map(MvnProxy::new).toArray(MvnProxy[]::new);
    }
}
