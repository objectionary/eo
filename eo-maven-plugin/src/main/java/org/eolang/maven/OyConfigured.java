/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import org.apache.maven.settings.Settings;
import org.cactoos.Scalar;
import org.cactoos.scalar.Unchecked;

/**
 * The Objectionary a Mojo pulls from, as the build configures it.
 *
 * <p>Answers of the remote are cached and indexed, and the remote itself is
 * reached through the active proxy of the Maven settings. The chain is built
 * on the first request, not in the constructor, because both the commit hash
 * and the settings are injected into the Mojo after this object is made. A
 * Mojo keeps it as a {@link Scalar}, so that a test may hand the Mojo a fake
 * one instead.</p>
 *
 * @since 0.62.0
 */
final class OyConfigured implements Scalar<Objectionary> {

    /**
     * The hash of the commit to pull from.
     */
    private final Scalar<CommitHash> hash;

    /**
     * The settings of the build, with the proxies in them.
     */
    private final Scalar<Settings> settings;

    /**
     * The guard of the chain, since Mojos pull in parallel.
     */
    private final Lock guard;

    /**
     * The chain, built on first request.
     */
    private Objectionary origin;

    /**
     * Ctor.
     *
     * @param hash The hash of the commit to pull from
     * @param settings The settings of the build
     */
    OyConfigured(final Scalar<CommitHash> hash, final Scalar<Settings> settings) {
        this.hash = hash;
        this.settings = settings;
        this.guard = new ReentrantLock();
    }

    @Override
    public Objectionary value() {
        this.guard.lock();
        try {
            if (this.origin == null) {
                this.origin = new OyIndexed(
                    new OyCached(
                        new OyRemote(
                            new Unchecked<>(this.hash).value(),
                            new Proxies(new Unchecked<>(this.settings).value()).value()
                        )
                    )
                );
            }
        } finally {
            this.guard.unlock();
        }
        return this.origin;
    }
}
