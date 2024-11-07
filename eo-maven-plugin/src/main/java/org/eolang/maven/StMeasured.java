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
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

/**
 * Shift that measures and saves stats into a file.
 *
 * @since 0.30
 */
public final class StMeasured implements Shift {

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
    public StMeasured(final Shift shift, final Path log) {
        this.origin = shift;
        this.path = log;
    }

    @Override
    public String uid() {
        return this.origin.uid();
    }

    @Override
    @SuppressWarnings("PMD.PrematureDeclaration")
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
                ).getBytes(),
                StandardOpenOption.APPEND,
                StandardOpenOption.CREATE
            );
        } catch (final IOException ex) {
            throw new IllegalArgumentException(ex);
        }
        return out;
    }
}
