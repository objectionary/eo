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
 * Object name with default hash.
 *
 * @since 0.29.6
 */
public final class OnDefault implements ObjectName {

    /**
     * Object with or without hash.
     */
    private final String object;

    /**
     * Default hash.
     */
    private final CommitHash hsh;

    /**
     * Ctor.
     * Please use the constructor for tests only because it can't guarantee
     * that {@code hash} is actually hash but not a random string.
     * @param object Object full name with a version or not.
     * @param hash Default hash is a version in full name is absent.
     */
    public OnDefault(final String object, final String hash) {
        this(object, new CommitHash.ChConstant(hash));
    }

    /**
     * Ctor.
     * @param object Object full name with a version or not.
     * @param def Default hash if a version in full name is absent.
     */
    public OnDefault(final String object, final CommitHash def) {
        this.object = object;
        this.hsh = def;
    }

    @Override
    public String value() {
        return this.split()[0];
    }

    @Override
    public CommitHash hash() {
        return new CommitHash.ChConstant(this.split()[1]);
    }

    @Override
    public String toString() {
        return String.join(
            OnVersioned.DELIMITER,
            this.split()[0],
            this.split()[1]
        );
    }

    /**
     * Split a given object.
     * @return Split object to name and hash.
     */
    private String[] split() {
        String[] splt = this.object.split(OnVersioned.DELIMITER);
        if (splt.length == 1) {
            splt = new String[]{splt[0], this.hsh.value()};
        }
        return splt;
    }
}
