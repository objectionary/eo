/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.parser;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import java.util.function.Consumer;
import org.eolang.parser.xmir.Xmir;

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
                    "Eo representation of the parsed xml: %n%s",
                    new Xmir.Default(xml).toEO()
                )
            );
            throw new IllegalStateException(
                String.format("Shift '%s' failed", this.origin),
                ex
            );
        }
    }
}
