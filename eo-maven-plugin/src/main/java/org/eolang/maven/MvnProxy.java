/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.net.InetSocketAddress;
import java.net.Proxy;
import java.util.Arrays;

/**
 * One active proxy of Maven settings, carrying the excluded hosts that a
 * plain {@link Proxy} drops.
 * @since 0.73.4
 * @todo #7257:40min Honour the proxy credentials too. A proxy with a
 *  {@code username} and {@code password} in {@code settings.xml} still gets
 *  407 on every request, because nothing answers its authentication
 *  challenge. Add a {@code java.net.Authenticator} built from these two
 *  fields and hand it to the {@code HttpClient} in {@code OyRemote}. The
 *  {@code protocol} field is a separate matter: {@code java.net.http} only
 *  speaks to HTTP proxies, so a {@code socks5} one has to either fail loudly
 *  or go through the {@code socksProxyHost} system properties.
 */
final class MvnProxy {

    /**
     * The Maven proxy this wraps.
     */
    private final org.apache.maven.settings.Proxy origin;

    /**
     * Ctor.
     * @param origin The Maven proxy this wraps
     */
    MvnProxy(final org.apache.maven.settings.Proxy origin) {
        this.origin = origin;
    }

    /**
     * The Java proxy this settles to.
     * @return The Java proxy
     */
    Proxy address() {
        return new Proxy(
            Proxy.Type.HTTP,
            new InetSocketAddress(this.origin.getHost(), this.origin.getPort())
        );
    }

    /**
     * Whether the given host is excluded from this proxy by its
     * {@code nonProxyHosts} pattern, the same {@code |}-separated,
     * {@code *}-wildcard glob syntax the JDK's own {@code http.nonProxyHosts}
     * property uses.
     * @param host The host a request is bound for
     * @return True when the host must be reached directly
     */
    boolean excludes(final String host) {
        final String hosts = this.origin.getNonProxyHosts();
        return hosts != null && Arrays.stream(hosts.split("\\|")).anyMatch(
            pattern -> host.matches(pattern.trim().replace(".", "\\.").replace("*", ".*"))
        );
    }
}
