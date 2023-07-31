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

import org.cactoos.Scalar;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;
import org.eolang.maven.hash.CommitHash;

/**
 * Object name with default hash.
 *
 * @since 0.29.6
 */
public final class ObNmDefault implements ObjectName {

    /**
     * Full name split.
     */
    private final Unchecked<String[]> split;

    /**
     * Ctor.
     *
     * @param object Object full name (with version or not).
     * @param def Default hash if version in full name is absent.
     */
    public ObNmDefault(final String object, final CommitHash def) {
        this.split = new Unchecked<>(
            new Sticky<>(ObNmDefault.split(object, def))
        );
    }

    @Override
    public String value() {
        return this.split.value()[0];
    }

    @Override
    public CommitHash hash() {
        return new CommitHash.ChConstant(this.split.value()[1]);
    }

    @Override
    public String asString() {
        return String.join("|", this.split.value()[0], this.split.value()[1]);
    }

    @Override
    public String toString() {
        return this.asString();
    }

    /**
     * Split given object.
     * @param object Object.
     * @param hash Default hash.
     * @return Split object.
     */
    private static Scalar<String[]> split(final String object, final CommitHash hash) {
        return () -> {
            String[] splt = object.split("\\|");
            if (splt.length == 1) {
                splt = new String[]{splt[0], hash.value()};
            }
            return splt;
        };
    }
}
