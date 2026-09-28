/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import org.cactoos.Fallback;
import org.cactoos.Input;
import org.cactoos.scalar.IoChecked;
import org.cactoos.scalar.ScalarWithFallback;

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
     *
     * @param objectionary Objectionary
     */
    OyIndexed(final Objectionary objectionary) {
        this(objectionary, new ObjectsIndex());
    }

    /**
     * Ctor.
     *
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

    @Override
    public boolean contains(final String name) throws IOException {
        return new IoChecked<>(
            new ScalarWithFallback<>(
                () -> this.index.contains(name),
                new Fallback.From<>(
                    Exception.class,
                    ex -> {
                        Logger.warn(
                            this,
                            "Failed to check object %s in objectionary index: %[exception]s",
                            name,
                            ex
                        );
                        return this.delegate.contains(name);
                    }
                )
            )
        ).value();
    }

    @Override
    public boolean isDirectory(final String name) throws IOException {
        return new IoChecked<>(
            new ScalarWithFallback<>(
                () -> !this.index.contains(name) && this.delegate.isDirectory(name),
                new Fallback.From<>(
                    Exception.class,
                    ex -> {
                        Logger.warn(
                            this,
                            "Failed to check object %s in objectionary index: %[exception]s. Try to check via delegate",
                            name,
                            ex
                        );
                        return !this.delegate.contains(name) && this.delegate.isDirectory(name);
                    }
                )
            )
        ).value();
    }

    @Override
    public Iterable<String> children(final String pkg) throws IOException {
        return new IoChecked<>(
            new ScalarWithFallback<>(
                () -> this.index.children(pkg),
                new Fallback.From<>(Exception.class, ex -> this.delegate.children(pkg))
            )
        ).value();
    }
}
