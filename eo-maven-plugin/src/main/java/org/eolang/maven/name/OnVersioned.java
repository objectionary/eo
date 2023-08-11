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
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.hash.CommitHashesMap;

/**
 * Object name versioned.
 * This is object name that parses raw sting like:
 * - "org.eolang.text#0.1.0" into "org.eolang.text"
 *   and "4b19944"
 * - "org.eolang.string#a1b2c3d" into "org.eolang.string"
 *   and "be83d9a"
 * Pay attention that versions transformed into hashes.
 * If a version is not provided - behaves like {@link OnUnversioned}.
 *
 * @since 0.30
 * @todo #2376:90min Remove VersionsMojo.
 *  It is not used anymore. Remove it and all its dependencies from all the places.
 *  We need to apply {@link OnVersioned} in {@link org.eolang.maven.DiscoverMojo}
 *  and replace all the tests from VersionsMojoTest to DiscoverMojoTest.
 *  Also we need to enable the next tests:
 *  - {@link org.eolang.maven.ProbeMojoTest#findsProbesWithVersionsInDifferentObjectionaries()}
 *  - {@link org.eolang.maven.PullMojoTest#pullsProbedVersionedObjectsFromDifferentObjectionaries()}
 *  - {@link org.eolang.maven.DiscoverMojoTest#discoversWithSeveralObjectsWithDifferentVersions()}
 *  - {@link org.eolang.maven.DiscoverMojoTest#discoversWithVersions()}
 *  Don't forget to remove that puzzle after all.
 * @todo #2376:90min Choose correct DELIMITER for a version.
 *  I tried to apply # delimiter everywhere and failed because we use hash (#) for comments.
 *  Hence, it conflicts with the new delimiter. We need to choose another delimiter character
 *  and replace all the places where we use the old delimiters | and # with the new one.
 */
public final class OnVersioned implements ObjectName {

    /**
     * Delimiter between name and hash in EO object name.
     */
    public static final String DELIMITER = "#";

    /**
     * Default hashes.
     */
    private static final Map<String, CommitHash> DEFAULT = new CommitHashesMap();

    /**
     * Raw string.
     * Examples:
     * - "org.eolang.text#0.1.0"
     * - "org.eolang.string#1.23.1"
     * - "org.eolang.math#3.3.3"
     */
    private final String raw;

    /**
     * All hashes.
     */
    private final Map<String, ? extends CommitHash> hashes;

    /**
     * Constructor.
     * @param origin Raw string.
     */
    public OnVersioned(final String origin) {
        this(origin, OnVersioned.DEFAULT);
    }

    /**
     * Constructor.
     * @param origin Raw string.
     * @param all All hashes.
     */
    OnVersioned(
        final String origin,
        final Map<String, ? extends CommitHash> all
    ) {
        this.raw = origin;
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
        if (this.raw.contains(OnVersioned.DELIMITER)) {
            result = String.join(
                "",
                this.value(),
                OnVersioned.DELIMITER,
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
        return this.raw.split(OnVersioned.DELIMITER);
    }
}

