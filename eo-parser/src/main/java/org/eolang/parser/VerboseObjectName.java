/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.nio.file.Path;
import java.util.function.Supplier;
import org.cactoos.Proc;

/**
 * Verbose object name.
 * This object is supposed to be used together with {@link ObjectName}.
 *
 * @since 0.56.5
 */
public final class VerboseObjectName implements Supplier<String> {

    /**
     * Origin.
     */
    private final Supplier<String> origin;

    /**
     * Program path.
     */
    private final Path program;

    /**
     * Ctor.
     *
     * @param orgn Origin
     * @param source Program path
     */
    public VerboseObjectName(final Supplier<String> orgn, final Path source) {
        this.origin = orgn;
        this.program = source;
    }

    @Override
    public String get() {
        try {
            return this.origin.get();
        } catch (final IllegalStateException exception) {
            throw new IllegalStateException(
                String.format(
                    "Source file '%s' encountered some problems, broken syntax?", this.program
                ),
                exception
            );
        }
    }
}
