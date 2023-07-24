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

import org.eolang.maven.hash.CommitHash;

/**
 * Objectionaries.
 * @since 0.29.6
 */
public interface Objectionaries {
    /**
     * Objectionaries with given objectionary by given hash.
     * @param hash Hash as String.
     * @param objectionary Objectionary.
     * @return Objectionaries with new objectionary.
     */
    Objectionaries with(String hash, Objectionary objectionary);

    /**
     * Objectionaries with given objectionary by given hash.
     * @param hash Hash as {@link CommitHash}.
     * @param objectionary Objectionary.
     * @return Objectionaries with new objectionary.
     */
    default Objectionaries with(CommitHash hash, Objectionary objectionary) {
        return this.with(hash.value(), objectionary);
    }

    /**
     * Whether objectionaries has objectionary by given hash.
     * @param hash Hash.
     * @return If objectionary has objectionary by given hash.
     */
    boolean has(String hash);

    /**
     * Whether objectionaries has objectionary by given hash.
     * @param hash Hash.
     * @return If objectionary has objectionary by given hash.
     */
    default boolean has(CommitHash hash) {
        return this.has(hash.value());
    }

    /**
     * Get objectionary by given hash.
     * @param hash Hash.
     * @return Objectionary by hash.
     */
    Objectionary get(String hash);

    /**
     * Get objectionary by given hash.
     * @param hash Hash as {@link CommitHash}.
     * @return Objectionary by hash.
     */
    default Objectionary get(CommitHash hash) {
        return this.get(hash.value());
    }
}
