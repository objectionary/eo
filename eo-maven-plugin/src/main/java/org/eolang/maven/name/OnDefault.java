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
package org.eolang.maven.name;

import org.eolang.maven.hash.CommitHash;

/**
 * Default object name that just split given raw string to name and hash.
 *
 * @since 0.31.0
 */
public final class OnDefault implements ObjectName {

    /**
     * Delimited name.
     */
    private DelimitedName name;

    /**
     * Ctor.
     * @param object Object name with hash.
     */
    public OnDefault(final String object) {
        this(new DelimitedName(object));
    }

    /**
     * Ctor.
     * @param name Object name with hash.
     */
    public OnDefault(final DelimitedName name) {
        this.name = name;
    }

    @Override
    public String value() {
        return this.name.title();
    }

    @Override
    public CommitHash hash() {
        return new CommitHash.ChConstant(this.name.label().get());
    }

    @Override
    public String toString() {
        return this.name.toString();
    }
}
