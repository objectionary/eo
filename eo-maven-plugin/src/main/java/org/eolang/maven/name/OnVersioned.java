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

import java.util.Optional;
import org.cactoos.Scalar;
import org.cactoos.scalar.Unchecked;
import org.eolang.maven.hash.CommitHash;

/**
 * Object name with default hash.
 * If a given object does not contain a version - put the default one.
 *
 * @since 0.29.6
 */
public final class OnVersioned implements ObjectName {

    /**
     * Object name with or without hash.
     */
    private final Unchecked<DelimitedName> name;

    /**
     * Object parsed with or without hash.
     */
    private final DelimitedName parsed;

    /**
     * Ctor.
     * @param origin Origin object name
     * @param hash Default hash if a version in full name is absent.
     */
    public OnVersioned(final ObjectName origin, final CommitHash hash) {
        this(origin, () -> hash);
    }

    /**
     * Ctor.
     * @param origin Origin object name
     * @param hash Default hash if a version in full name is absent.
     */
    public OnVersioned(final ObjectName origin, final Scalar<CommitHash> hash) {
        this(new Unchecked<>(origin::toString), new Unchecked<>(hash));
    }

    /**
     * Ctor.
     * Please use the constructor for tests only because it can't guarantee
     * that {@code hash} is actually hash but not a random string.
     * @param object Object full name with a version or not.
     * @param hash Default hash if a version in full name is absent.
     */
    public OnVersioned(final String object, final String hash) {
        this(object, new CommitHash.ChConstant(hash));
    }

    /**
     * Ctor.
     * @param object Object full name with a version or not.
     * @param def Default hash if a version in full name is absent.
     */
    public OnVersioned(final String object, final CommitHash def) {
        this(new Unchecked<>(() -> object), new Unchecked<>(() -> def));
    }

    /**
     * Ctor.
     * @param object Object full name with a version or not as scalar.
     * @param def Default hash if a version in full name is absent.
     */
    private OnVersioned(final Unchecked<String> object, final Unchecked<CommitHash> def) {
        this.parsed = new DelimitedName(object.value());
        this.name = new Unchecked<>(
            () -> {
                final DelimitedName res;
                if (this.parsed.label().isPresent()) {
                    res = this.parsed;
                } else {
                    res = new DelimitedName(object.value(), Optional.of(def.value().value()));
                }
                return res;
            }
        );
    }

    @Override
    public String value() {
        return this.name.value().title();
    }

    @Override
    public CommitHash hash() {
        return new CommitHash.ChConstant(this.name.value().label().orElse(""));
    }

    @Override
    public String toString() {
        return String.valueOf(this.name.value());
    }
}
