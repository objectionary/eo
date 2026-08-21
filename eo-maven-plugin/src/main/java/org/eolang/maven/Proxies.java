/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.apache.maven.settings.Settings;
import org.cactoos.Scalar;

/**
 * Active proxies of Maven settings, translated to the ones of Java.
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
        return this.settings.getProxies()
            .stream()
            .filter(org.apache.maven.settings.Proxy::isActive)
            .map(MvnProxy::new)
            .toArray(MvnProxy[]::new);
    }
}
