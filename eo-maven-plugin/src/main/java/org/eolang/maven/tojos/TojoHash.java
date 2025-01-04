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
package org.eolang.maven.tojos;

import java.util.function.Supplier;

/**
 * Optional tojo hash.
 * Returns tojo hash if exists or empty string otherwise.
 * @since 0.41
 */
public final class TojoHash implements Supplier<String> {
    /**
     * Tojo.
     */
    private final ForeignTojo tojo;

    /**
     * Ctor.
     * @param tjo Foreign tojo
     */
    public TojoHash(final ForeignTojo tjo) {
        this.tojo = tjo;
    }

    @Override
    public String get() {
        final String hash;
        if (this.tojo.hasHash()) {
            hash = this.tojo.hash();
        } else {
            hash = "";
        }
        return hash;
    }
}
