/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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

package org.eolang;

/**
 * Wrapper for {@link Attr}.
 *
 * @since 0.36.0
 * @checkstyle DesignForExtensionCheck (100 lines)
 */
public abstract class AtEnvelope implements Attr {
    /**
     * Original attribute.
     */
    private final Attr origin;

    /**
     * Ctor.
     * @param attr Attribute
     */
    public AtEnvelope(final Attr attr) {
        this.origin = attr;
    }

    @Override
    public Attr copy(final Phi self) {
        return this.origin.copy(self);
    }

    @Override
    public Phi get() {
        return this.origin.get();
    }

    @Override
    public boolean put(final Phi phi) {
        return this.origin.put(phi);
    }
}
