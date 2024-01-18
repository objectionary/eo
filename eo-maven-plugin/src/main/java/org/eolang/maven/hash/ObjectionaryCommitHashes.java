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

import java.net.URL;
import org.cactoos.scalar.Unchecked;
import org.cactoos.text.TextEnvelope;
import org.cactoos.text.TextOf;

/**
 * CommitHashes which we download from Objectionary.
 *
 * @since 0.30
 */
final class ObjectionaryCommitHashes extends TextEnvelope {

    /**
     * Tags.
     */
    private static final String HOME = "https://home.objectionary.com/tags.txt";

    /**
     * Constructor.
     */
    ObjectionaryCommitHashes() {
        this(ObjectionaryCommitHashes.HOME);
    }

    /**
     * Constructor.
     * @param tags The url from which to download tags list.
     */
    private ObjectionaryCommitHashes(final String tags) {
        this(new Unchecked<>(() -> new URL(tags)).value());
    }

    /**
     * Constructor.
     * @param tags The url from which to download tags list.
     */
    private ObjectionaryCommitHashes(final URL tags) {
        super(new TextOf(tags));
    }
}
