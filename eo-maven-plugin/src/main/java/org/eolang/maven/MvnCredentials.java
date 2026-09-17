/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.net.Authenticator;
import java.net.PasswordAuthentication;

/**
 * What a proxy of Maven settings answers its authentication challenge with.
 *
 * <p>Only the challenge of a proxy is answered, never the one of the server
 * behind it: the credentials of {@code settings.xml} are the proxy's own, and
 * handing them to whoever asks would send them to a host that never asked for
 * them.</p>
 *
 * @since 0.74.0
 */
public final class MvnCredentials extends Authenticator {

    /**
     * The user name.
     */
    private final String user;

    /**
     * The password.
     */
    private final String secret;

    /**
     * Ctor.
     *
     * @param name The user name
     * @param password The password
     */
    public MvnCredentials(final String name, final String password) {
        super();
        this.user = name;
        this.secret = password;
    }

    @Override
    public PasswordAuthentication getPasswordAuthentication() {
        final PasswordAuthentication found;
        if (this.getRequestorType() == RequestorType.PROXY) {
            final String password;
            if (this.secret == null) {
                password = "";
            } else {
                password = this.secret;
            }
            found = new PasswordAuthentication(this.user, password.toCharArray());
        } else {
            found = null;
        }
        return found;
    }
}
