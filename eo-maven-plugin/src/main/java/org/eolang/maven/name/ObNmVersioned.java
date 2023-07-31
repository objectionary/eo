/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
package org.eolang.maven.name;

import org.eolang.maven.hash.CommitHash;

/**
 * Versioned object full name.
 *
 * @since 0.29.6
 */
public final class ObNmVersioned implements ObjectName {

    /**
     * Origin.
     */
    private final ObjectName origin;

    /**
     * Use versions or not.
     */
    private final boolean versioned;

    /**
     * Ctor.
     * @param src Origin object name.
     * @param ver Use versions or not.
     */
    public ObNmVersioned(final ObjectName src, final boolean ver) {
        this.origin = src;
        this.versioned = ver;
    }

    @Override
    public String asString() {
        final String result;
        if (this.versioned) {
            result = this.origin.asString();
        } else {
            result = this.value();
        }
        return result;
    }

    @Override
    public String value() {
        return this.origin.value();
    }

    @Override
    public CommitHash hash() {
        return this.origin.hash();
    }
}
