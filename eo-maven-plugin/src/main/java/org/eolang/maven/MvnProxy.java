/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.net.Authenticator;
import java.net.InetSocketAddress;
import java.net.PasswordAuthentication;
import java.net.Proxy;
import java.util.Arrays;

/**
 * One active proxy of Maven settings, carrying the excluded hosts that a
 * plain {@link Proxy} drops.
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
     * The authenticator carrying this proxy's credentials.
     * @return The authenticator
     */
    Authenticator authenticator() {
        final String username = this.origin.getUsername();
        final String password = this.origin.getPassword();
        return new Authenticator() {
            @Override
            protected PasswordAuthentication getPasswordAuthentication() {
                PasswordAuthentication result = null;
                if (
                    this.getRequestorType() == RequestorType.PROXY
                        && username != null && password != null
                ) {
                    result = new PasswordAuthentication(
                        username, password.toCharArray()
                    );
                }
                return result;
            }
        };
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
