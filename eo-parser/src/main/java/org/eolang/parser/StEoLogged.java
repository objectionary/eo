/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import java.util.function.Consumer;

/**
 * Shift that logs the EO representation of the XML before throwing an exception.
 *
 * @since 0.30
 */
final class StEoLogged implements Shift {

    /**
     * Origin shift.
     */
    private final Shift origin;

    /**
     * Logger.
     */
    private final Consumer<? super String> logger;

    /**
     * Ctor.
     * @param shift Origin shift
     */
    StEoLogged(final Shift shift) {
        this(shift, message -> Logger.error(StEoLogged.class, message));
    }

    /**
     * Ctor.
     * @param origin Origin shift
     * @param logger Logger
     */
    StEoLogged(final Shift origin, final Consumer<? super String> logger) {
        this.origin = origin;
        this.logger = logger;
    }

    @Override
    public String uid() {
        return this.origin.uid();
    }

    @Override
    @SuppressWarnings("PMD.AvoidCatchingGenericException")
    public XML apply(final int position, final XML xml) {
        try {
            return this.origin.apply(position, xml);
            // @checkstyle IllegalCatchCheck (1 line)
        } catch (final RuntimeException ex) {
            this.logger.accept(
                String.format(
                    "EO representation of the parsed XML: %n%s",
                    new Xmir(xml).toEO()
                )
            );
            throw new IllegalStateException(
                String.format("Shift '%s' failed", this.origin),
                ex
            );
        }
    }
}
