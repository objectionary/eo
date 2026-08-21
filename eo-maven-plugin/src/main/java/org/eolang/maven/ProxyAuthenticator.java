/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.net.Authenticator;
import java.net.PasswordAuthentication;

/**
 * Answers a Maven proxy's own credentials whenever the JDK's HTTP client
 * challenges for them while negotiating with it.
 * @since 0.73.4
 */
final class ProxyAuthenticator extends Authenticator {

    /**
     * The proxy whose credentials this answers with.
     */
    private final MvnProxy proxy;

    /**
     * Ctor.
     * @param proxy The proxy whose credentials this answers with
     */
    ProxyAuthenticator(final MvnProxy proxy) {
        this.proxy = proxy;
    }

    @Override
    protected PasswordAuthentication getPasswordAuthentication() {
        return this.proxy.credentials();
    }
}
