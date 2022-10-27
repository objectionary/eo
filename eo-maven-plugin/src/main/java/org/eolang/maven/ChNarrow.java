/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
package org.eolang.maven;

import java.util.Objects;

/**
 * Short version of hash.
 *
 * @since 0.28.11
 */
final class ChNarrow implements CommitHash {

    /**
     * Delegate.
     */
    private final CommitHash full;

    /**
     * The main constructor.
     *
     * @param full Delegate
     */
    ChNarrow(final CommitHash full) {
        this.full = full;
    }

    @Override
    public String value() {
        return this.validHash().substring(0, 7);
    }

    /**
     * Valid hash.
     *
     * @return Full valid hash.
     */
    private String validHash() {
        final String hash = this.full.value();
        if (Objects.isNull(hash)) {
            throw new IllegalArgumentException(
                String.format("Hash can't be null. The delegate %s", this.full)
            );
        } else if (hash.isEmpty()) {
            throw new IllegalArgumentException(
                String.format("Hash can't empty. The delegate %s", this.full)
            );
        } else if (hash.length() <= 7) {
            throw new IllegalArgumentException(
                String.format(
                    "%s, but was %d, hash %s, delegate %s",
                    "The hash size has to be grater or equals then 7",
                    hash.length(),
                    hash,
                    this.full
                ));
        }
        return hash;
    }
}
