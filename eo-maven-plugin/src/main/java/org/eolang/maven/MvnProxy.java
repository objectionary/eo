/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.net.Authenticator;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.util.Arrays;
import java.util.Optional;
import java.util.regex.Pattern;

/**
 * One active proxy of Maven settings, carrying the excluded hosts that a
 * plain {@link Proxy} drops.
 *
 * @since 0.73.4
 */
final class MvnProxy {

    /**
     * The Maven proxy this wraps.
     */
    private final org.apache.maven.settings.Proxy origin;

    /**
     * Ctor.
     *
     * @param origin The Maven proxy this wraps
     */
    MvnProxy(final org.apache.maven.settings.Proxy origin) {
        this.origin = origin;
    }

    /**
     * The Java proxy this settles to.
     *
     * @return The Java proxy
     */
    Proxy address() {
        return new Proxy(
            Proxy.Type.HTTP,
            new InetSocketAddress(this.origin.getHost(), this.origin.getPort())
        );
    }

    /**
     * What answers this proxy's authentication challenge.
     *
     * <p>A proxy that wants a user name and a password gets a 407 on every
     * request while nothing answers it, and the two fields sit unread in
     * {@code settings.xml}. A proxy that wants neither is left alone, since
     * an authenticator with nothing to say only makes the client ask.</p>
     *
     * @return The credentials, empty when the settings carry none
     */
    Optional<Authenticator> credentials() {
        final String name = this.origin.getUsername();
        final Optional<Authenticator> found;
        if (name == null || name.isEmpty()) {
            found = Optional.empty();
        } else {
            found = Optional.of(new MvnCredentials(name, this.origin.getPassword()));
        }
        return found;
    }

    /**
     * Whether the given host is excluded from this proxy by its
     * {@code nonProxyHosts} pattern, the same {@code |}-separated,
     * {@code *}-wildcard glob syntax the JDK's own {@code http.nonProxyHosts}
     * property uses.
     *
     * <p>A host name means the same thing however it is capitalised (RFC
     * 4343), and the JDK reads the property that way too, so a pattern is
     * matched without regard to case: an excluded host stays excluded when
     * the repository URL spells it with a capital letter.</p>
     *
     * @param host The host a request is bound for
     * @return True when the host must be reached directly
     */
    boolean excludes(final String host) {
        final String hosts = this.origin.getNonProxyHosts();
        return hosts != null && Arrays.stream(hosts.split("\\|")).anyMatch(
            pattern -> Pattern.compile(
                pattern.trim().replace(".", "\\.").replace("*", ".*"),
                Pattern.CASE_INSENSITIVE
            ).matcher(host).matches()
        );
    }
}
