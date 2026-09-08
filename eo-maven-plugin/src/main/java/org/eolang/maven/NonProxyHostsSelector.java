/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.net.Proxy;
import java.net.ProxySelector;
import java.net.SocketAddress;
import java.net.URI;
import java.util.Collections;
import java.util.List;

/**
 * A {@link ProxySelector} of one Maven proxy, honouring its
 * {@code nonProxyHosts} exclusion instead of sending every request through
 * the proxy regardless of what the user excluded. A failed connection is
 * only logged: {@link OyRemote} retries the whole request itself, so there
 * is no per-address bookkeeping for this selector to keep.
 *
 * @since 0.73.4
 */
final class NonProxyHostsSelector extends ProxySelector {

    /**
     * The proxy this selects, or steps around.
     */
    private final MvnProxy proxy;

    /**
     * Ctor.
     *
     * @param proxy The proxy this selects, or steps around
     */
    NonProxyHostsSelector(final MvnProxy proxy) {
        this.proxy = proxy;
    }

    @Override
    public List<Proxy> select(final URI uri) {
        final List<Proxy> result;
        if (this.proxy.excludes(uri.getHost())) {
            result = Collections.singletonList(Proxy.NO_PROXY);
        } else {
            result = Collections.singletonList(this.proxy.address());
        }
        return result;
    }

    @Override
    public void connectFailed(final URI uri, final SocketAddress addr, final IOException error) {
        Logger.debug(this, "Failed to reach %s through %s: %[exception]s", uri, addr, error);
    }
}
