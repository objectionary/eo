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

import java.util.Map;
import java.util.Optional;
import org.cactoos.Scalar;
import org.cactoos.scalar.Unchecked;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.hash.CommitHashesMap;

/**
 * Object name replaced.
 * This is object name that parses raw sting like:
 * - "org.eolang.text|0.1.0" into "org.eolang.text"
 *   and "4b19944"
 * - "org.eolang.string|a1b2c3d" into "org.eolang.string"
 *   and "be83d9a"
 * Pay attention to that versions transformed into narrow hashes.
 * If a version is not provided - behaves like {@link OnUnversioned}.
 *
 * @since 0.30
 */
public final class OnReplaced implements ObjectName {

    /**
     * Default hashes.
     */
    private static final Map<String, CommitHash> DEFAULT = new CommitHashesMap();

    /**
     * Hash if the {@link DelimitedName#label()} is empty.
     */
    private static final CommitHash EMPTY_HASH = new CommitHash.ChConstant("");

    /**
     * Name that represents value as title and version as label.
     * Examples:
     * - "org.eolang.text", "0.1.0"
     * - "org.eolang.string", "1.23.1"
     * - "org.eolang.math", "3.3.3"
     */
    private final DelimitedName name;

    /**
     * Name for text representation with version hash as label.
     */
    private final Unchecked<DelimitedName> concat;

    /**
     * All hashes.
     */
    private final Map<String, ? extends CommitHash> hashes;

    /**
     * Constructor.
     * @param origin Origin object name.
     */
    public OnReplaced(final ObjectName origin) {
        this(origin, OnReplaced.DEFAULT);
    }

    /**
     * Constructor.
     * @param origin Origin object name.
     * @param all All hashes.
     */
    public OnReplaced(
        final ObjectName origin,
        final Map<String, ? extends CommitHash> all
    ) {
        this(origin::toString, all);
    }

    /**
     * Constructor.
     * @param origin Raw string.
     */
    public OnReplaced(final String origin) {
        this(origin, OnReplaced.DEFAULT);
    }

    /**
     * Constructor.
     * @param origin Raw string.
     * @param all All hashes.
     */
    public OnReplaced(
        final String origin,
        final Map<String, ? extends CommitHash> all
    ) {
        this(() -> origin, all);
    }

    /**
     * Constructor.
     * @param origin Raw string as scalar.
     * @param all All hashes.
     */
    OnReplaced(
        final Scalar<String> origin,
        final Map<String, ? extends CommitHash> all
    ) {
        this.name = new DelimitedName(origin);
        this.concat = new Unchecked<>(
            () -> new DelimitedName(
                this.name.title(),
                this.name.label()
                    .map(
                        label -> this.hash().value()
                    )
            )
        );
        this.hashes = all;
    }

    @Override
    public String value() {
        return this.name.title();
    }

    @Override
    public CommitHash hash() {
        final Optional<CommitHash> label = this.name.label().map(this.hashes::get);
        return label.orElse(OnReplaced.EMPTY_HASH);
    }

    @Override
    public String toString() {
        return String.valueOf(this.concat.value());
    }
}

