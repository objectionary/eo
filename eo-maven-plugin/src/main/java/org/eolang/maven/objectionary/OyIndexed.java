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
package org.eolang.maven.objectionary;

import com.jcabi.log.Logger;
import java.io.IOException;
import org.cactoos.Input;

/**
 * Objectionary with index.
 *
 * @since 0.29
 */
public final class OyIndexed implements Objectionary {

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
    public OyIndexed(final Objectionary objectionary) {
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
}
