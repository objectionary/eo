/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.net.InetSocketAddress;
import java.net.PasswordAuthentication;
import java.net.Proxy;
import java.util.Arrays;

/**
 * One active proxy of Maven settings, carrying the protocol, credentials
 * and excluded hosts that a plain {@link Proxy} drops.
 * @since 0.73.4
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
     * The Java proxy this settles to, typed by the configured protocol.
     * @return The Java proxy
     */
    Proxy address() {
        final String protocol = this.origin.getProtocol();
        final Proxy.Type type;
        if ("socks4".equalsIgnoreCase(protocol) || "socks5".equalsIgnoreCase(protocol)) {
            type = Proxy.Type.SOCKS;
        } else {
            type = Proxy.Type.HTTP;
        }
        return new Proxy(
            type, new InetSocketAddress(this.origin.getHost(), this.origin.getPort())
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

    /**
     * Whether this proxy carries credentials to authenticate with.
     * @return True when a username is configured
     */
    boolean secured() {
        final String user = this.origin.getUsername();
        return user != null && !user.isEmpty();
    }

    /**
     * The credentials to answer the proxy's authentication challenge with.
     * @return The credentials
     */
    PasswordAuthentication credentials() {
        final String pass = this.origin.getPassword();
        final char[] chars;
        if (pass == null) {
            chars = new char[0];
        } else {
            chars = pass.toCharArray();
        }
        return new PasswordAuthentication(this.origin.getUsername(), chars);
    }
}
