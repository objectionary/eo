/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import org.cactoos.Input;

/**
 * Objectionary with index.
 *
 * @since 0.29
 */
final class OyIndexed implements Objectionary {

    /**
     * Objectionary delegate.
     */
    private final Objectionary delegate;

    /**
     * Index to check.
     */
    private final ObjectsIndex index;

    /**
     * Ctor.
     * @param objectionary Objectionary
     */
    OyIndexed(final Objectionary objectionary) {
        this(objectionary, new ObjectsIndex());
    }

    /**
     * Ctor.
     * @param objectionary Objectionary
     * @param index Index
     */
    OyIndexed(final Objectionary objectionary, final ObjectsIndex index) {
        this.delegate = objectionary;
        this.index = index;
    }

    @Override
    public Input get(final String name) throws IOException {
        return this.delegate.get(name);
    }

    // @checkstyle IllegalCatchCheck (7 line)
    @SuppressWarnings("PMD.AvoidCatchingGenericException")
    @Override
    public boolean contains(final String name) throws IOException {
        boolean result;
        try {
            result = this.index.contains(name);
        } catch (final Exception ex) {
            Logger.warn(
                this,
                "Failed to check object %s in objectionary index: %[exception]s",
                name,
                ex
            );
            result = this.delegate.contains(name);
        }
        return result;
    }

    // @checkstyle IllegalCatchCheck (7 line)
    @SuppressWarnings("PMD.AvoidCatchingGenericException")
    @Override
    public boolean isDirectory(final String name) throws IOException {
        boolean result;
        try {
            result = !this.index.contains(name) && this.delegate.isDirectory(name);
        } catch (final Exception ex) {
            Logger.warn(
                this,
                "Failed to check object %s in objectionary index: %[exception]s. Try to check via delegate",
                name,
                ex
            );
            result = this.delegate.isDirectory(name);
        }
        return result;
    }
}
