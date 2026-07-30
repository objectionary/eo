/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.net.InetSocketAddress;
import java.net.Proxy;
import org.apache.maven.settings.Settings;
import org.cactoos.Scalar;

/**
 * Active proxies of Maven settings, translated to the ones of Java.
 * @since 0.62.0
 */
final class Proxies implements Scalar<Proxy[]> {

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
    public Proxy[] value() {
        return this.settings.getProxies()
            .stream()
            .filter(org.apache.maven.settings.Proxy::isActive).map(
                proxy -> new Proxy(
                    Proxy.Type.HTTP,
                    new InetSocketAddress(proxy.getHost(), proxy.getPort())
                )
            ).toArray(Proxy[]::new);
    }
}
