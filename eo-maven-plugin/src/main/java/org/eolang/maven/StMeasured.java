/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

/**
 * Shift that measures and saves stats into a file.
 *
 * @since 0.30
 */
final class StMeasured implements Shift {

    /**
     * Origin shift.
     */
    private final Shift origin;

    /**
     * Log file.
     */
    private final Path path;

    /**
     * Ctor.
     * @param shift Origin shift
     * @param log Log file
     */
    StMeasured(final Shift shift, final Path log) {
        this.origin = shift;
        this.path = log;
    }

    @Override
    public String uid() {
        return this.origin.uid();
    }

    @Override
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    public XML apply(final int position, final XML xml) {
        final long start = System.currentTimeMillis();
        final XML out = this.origin.apply(position, xml);
        try {
            Files.write(
                this.path,
                String.format(
                    "%s,%d\n",
                    this.origin.uid(),
                    System.currentTimeMillis() - start
                ).getBytes(StandardCharsets.UTF_8),
                StandardOpenOption.APPEND,
                StandardOpenOption.CREATE
            );
        } catch (final IOException ex) {
            throw new IllegalArgumentException(ex);
        }
        return out;
    }
}
