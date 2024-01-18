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
package org.eolang.maven.hash;

import org.cactoos.Text;
import org.cactoos.text.Sticky;
import org.cactoos.text.TextEnvelope;

/**
 * Commit hashes table as text from objectionary.
 * This class serves the purpose of the global cache in order to avoid
 * downloading the list of tags multiple times from objectionary.
 *
 * @since 0.29.6
 */
final class CommitHashesText extends TextEnvelope {

    /**
     * Cache.
     */
    private static final Text CACHE = new Sticky(new ObjectionaryCommitHashes());

    /**
     * Constructor.
     */
    CommitHashesText() {
        this(CommitHashesText.CACHE);
    }

    /**
     * Constructor.
     * @param text The text to of commit hashes.
     */
    private CommitHashesText(final Text text) {
        super(text);
    }
}
