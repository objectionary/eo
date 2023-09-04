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

import java.util.Map;
import org.cactoos.Scalar;
import org.cactoos.scalar.Sticky;
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
 * @todo #2376:90min Frontend and backend delimiters differ.
 *  I was confused with the delimiter '#' that we use in {@link OnReplaced} and delimiter which
 *  we use in the frontend. For example:
 *  - "org.eolang.text|0.1.0" - frontend
 *  - "org.eolang.text#0.1.0" - backend
 *  The problem here is that we use  the '|' delimiter on the frontend and '#' in the backend, but
 *  both of them mean the same thing - object name + version.
 *  I believe that we need to use the same symbol in both places, because it will be easier to
 *  understand the code. So, my suggestion to use '|' in both places.
 */
public final class OnReplaced implements ObjectName {

    /**
     * Delimiter between name and hash in EO object name.
     * @todo #2394:30min Hide static constant DELIMITER.
     *  We should hide static constant DELIMITER because it creates code duplication
     *  in many places. For example in {@link OnReplaced#split()}, {@link OnVersioned#split()},
     *  {@link OnDefault#split()} and
     *  {@link org.eolang.maven.Place#make(java.nio.file.Path, String)}.
     *  Apparently we have to create a class which will parse raw string into two parts value and
     *  optional version. Maybe this new class won't implement ObjectName interface.
     *  Why static fields are bad you can read here:
     *  - https://www.yegor256.com/2015/07/06/public-static-literals.html
     */
    public static final String DELIMITER = "|";

    /**
     * Default hashes.
     */
    private static final Map<String, CommitHash> DEFAULT = new CommitHashesMap();

    /**
     * Raw string.
     * Examples:
     * - "org.eolang.text|0.1.0"
     * - "org.eolang.string|1.23.1"
     * - "org.eolang.math|3.3.3"
     */
    private final Unchecked<String> raw;

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
        this.raw = new Unchecked<>(new Sticky<>(origin));
        this.hashes = all;
    }

    @Override
    public String value() {
        return this.split()[0];
    }

    @Override
    public CommitHash hash() {
        return this.hashes.get(this.split()[1]);
    }

    @Override
    public String toString() {
        final String result;
        if (this.raw.value().contains(OnReplaced.DELIMITER)) {
            result = String.join(
                "",
                this.value(),
                OnReplaced.DELIMITER,
                this.hash().value()
            );
        } else {
            result = this.value();
        }
        return result;
    }

    /**
     * Split raw string into name and hash.
     * @return Array of two elements: name and hash.
     */
    private String[] split() {
        return this.raw.value().split(String.format("\\%s", OnReplaced.DELIMITER));
    }
}

