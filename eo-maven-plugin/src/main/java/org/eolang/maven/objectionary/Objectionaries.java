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
package org.eolang.maven.objectionary;

import java.io.IOException;
import org.cactoos.Input;
import org.eolang.maven.hash.CommitHash;

/**
 * Many objectionaries for different hashes.
 * @since 0.29.6
 */
public interface Objectionaries {

    /**
     * Get object by hash and name.
     * @param hash Commit hash
     * @param name Object name
     * @return Object
     * @throws IOException If some I/O problem happens.
     */
    Input object(CommitHash hash, String name) throws IOException;

    /**
     * Check if object exists.
     * @param hash Commit hash
     * @param name Object name
     * @return True if object exists, false otherwise
     * @throws IOException If some I/O problem happens.
     */
    boolean contains(CommitHash hash, String name) throws IOException;

    /**
     * Fake objectionaries.
     * Contains only one default objectionary that will be returned by any hash.
     *
     * @since 0.29.6
     */
    class Fake implements Objectionaries {
        /**
         * Default objectionary.
         */
        private final Objectionary objry;

        /**
         * Ctor.
         */
        public Fake() {
            this(new Objectionary.Fake());
        }

        /**
         * Ctor.
         * @param objectionary Default objectionary
         */
        public Fake(final Objectionary objectionary) {
            this.objry = objectionary;
        }

        @Override
        public Input object(final CommitHash hash, final String name) throws IOException {
            return this.objry.get(name);
        }

        @Override
        public boolean contains(final CommitHash hash, final String name) throws IOException {
            return this.objry.contains(name);
        }
    }
}
