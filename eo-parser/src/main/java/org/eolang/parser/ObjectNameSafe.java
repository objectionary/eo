/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.nio.file.Path;
import java.util.function.Supplier;
import org.cactoos.Proc;
import org.cactoos.proc.UncheckedProc;

/**
 * Safe object name.
 * Object name that catches  and re-throws an exception if original one fails.
 * If {@link ObjectName} throw an exception, this object catches it, and executes supplied
 * procedure. This object is supposed to be used together with {@link ObjectName}.
 *
 * @since 0.56.5
 */
public final class ObjectNameSafe implements Supplier<String> {

    /**
     * Origin.
     */
    private final Supplier<String> origin;

    /**
     * If fails.
     */
    private final UncheckedProc<Exception> failure;

    /**
     * Ctor.
     * @param orgn Origin
     * @param source Program source path
     */
    public ObjectNameSafe(final Supplier<String> orgn, final Path source) {
        this(
            orgn,
            e -> {
                throw new IllegalStateException(
                    String.format(
                        "Source file '%s' encountered some problems, broken syntax?", source
                    ),
                    e
                );
            }
        );
    }

    /**
     * Ctor.
     * @param orgn Origin
     * @param fail If fails
     */
    public ObjectNameSafe(final Supplier<String> orgn, final Proc<Exception> fail) {
        this(orgn, new UncheckedProc<>(fail));
    }

    /**
     * Ctor.
     *
     * @param orgn Origin
     * @param fail If fails
     */
    public ObjectNameSafe(final Supplier<String> orgn, final UncheckedProc<Exception> fail) {
        this.origin = orgn;
        this.failure = fail;
    }

    @Override
    public String get() {
        String result;
        try {
            result = this.origin.get();
        } catch (final IllegalStateException exception) {
            this.failure.exec(exception);
            result = "";
        }
        return result;
    }
}
