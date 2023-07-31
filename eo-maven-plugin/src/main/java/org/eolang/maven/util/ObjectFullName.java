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
package org.eolang.maven.util;

import org.cactoos.Text;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;
import org.eolang.maven.hash.CommitHash;

/**
 * EO object full name.
 *
 * @since 0.29.6
 */
public final class ObjectFullName implements Text {

    /**
     * Full name split.
     */
    private final Unchecked<String[]> split;

    /**
     * Hash.
     */
    private final Unchecked<CommitHash> hsh;

    /**
     * Full name with version or not.
     */
    private final boolean versioned;

    /**
     * Ctor.
     * @param object Object full name (with version or not).
     * @param def Default hash if version in full name is absent.
     */
    public ObjectFullName(final String object, final CommitHash def) {
        this(object, def, true);
    }

    /**
     * Ctor.
     * @param object Object full name (with version or not).
     * @param def Default hash if version in full name is absent.
     * @param ver Should full name be with version or not.
     */
    public ObjectFullName(final String object, final CommitHash def, final boolean ver) {
        this.split = new Unchecked<>(
            new Sticky<>(
                () -> {
                    String[] splt = object.split("\\|");
                    if (splt.length == 1) {
                        splt = new String[]{splt[0], def.value()};
                    }
                    return splt;
                }
            )
        );
        this.hsh = new Unchecked<>(
            new Sticky<>(
                () -> new CommitHash.ChConstant(this.split.value()[1])
            )
        );
        this.versioned = ver;
    }

    /**
     * Name from the full name.
     * @return Name.
     */
    public String name() {
        return this.split.value()[0];
    }

    /**
     * Hash from the full name.
     * @return Hash.
     */
    public CommitHash hash() {
        return this.hsh.value();
    }

    @Override
    public String asString() {
        final String result;
        if (this.versioned) {
            result = String.join("|", this.split.value()[0], this.split.value()[1]);
        } else {
            result = this.split.value()[0];
        }
        return result;
    }

    @Override
    public String toString() {
        return this.asString();
    }
}
