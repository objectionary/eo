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
package org.eolang.maven.hash;

import java.nio.file.Path;

/**
 * Resolve {@link CommitHash} depending on the
 * parameters.
 *
 * @since 0.28.14
 * @todo #2299:30min Do we really need the class? It seems it's used only for
 *  the test purposes but with last updates it is no longer of value. Also it
 *  assumes that any of its arguments may be null which is a doubtful design
 */
public final class ChCompound implements CommitHash {

    /**
     * CommitHash that was chosen.
     */
    private final CommitHash delegate;

    /**
     * Ctor.
     *
     * @param file Hash from file
     * @param hash Hash by pattern
     * @param tag The Git hash to pull objects from
     */
    public ChCompound(final Path file, final String hash, final String tag) {
        this(ChCompound.chooseCh(file, hash, tag));
    }

    /**
     * Ctor.
     * @param origin The delegate.
     */
    private ChCompound(final CommitHash origin) {
        this.delegate = origin;
    }

    @Override
    public String value() {
        return this.delegate.value();
    }

    /**
     * Choose the right {@link CommitHash} implementation.
     * @param file Hash from file
     * @param hash Hash by pattern
     * @param tag The Git tag to pull objects from
     * @return The right {@link CommitHash} implementation.
     */
    private static CommitHash chooseCh(
        final Path file,
        final String hash,
        final String tag
    ) {
        final CommitHash ret;
        if (file == null && hash == null) {
            ret = new ChRemote(tag);
        } else if (hash == null) {
            ret = new ChText(file, tag);
        } else {
            ret = new ChPattern(hash, tag);
        }
        return new ChCached(ret);
    }

}
