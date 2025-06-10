/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.function.Supplier;
import org.cactoos.Proc;
import org.cactoos.proc.UncheckedProc;

/**
 * Object name failure.
 * This object is supposed to be used together with {@link ObjectName}.
 *
 * @since 0.56.5
 */
public final class ObjectNameFailure implements Supplier<String> {

    /**
     * Origin.
     */
    private final Supplier<String> origin;

    /**
     * If fails.
     */
    private final Proc<Exception> failure;

    /**
     * Ctor.
     *
     * @param orgn Origin
     * @param fail If fails
     */
    public ObjectNameFailure(final Supplier<String> orgn, final Proc<Exception> fail) {
        this.origin = orgn;
        this.failure = fail;
    }

    @Override
    public String get() {
        String result;
        try {
            result = this.origin.get();
        } catch (final IllegalStateException exception) {
            new UncheckedProc<>(this.failure).exec(exception);
            result = "";
        }
        return result;
    }
}
