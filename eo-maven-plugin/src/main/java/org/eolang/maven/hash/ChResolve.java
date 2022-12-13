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
package org.eolang.maven.hash;

import java.nio.file.Path;

/**
 * Resolve {@link CommitHash} depending on the
 * parameters.
 *
 * @since 0.28.11
 */
public final class ChResolve implements CommitHash {

    /**
     * Resolved {@link CommitHash}.
     */
    private final CommitHash hash;

    /**
     * Ctor.
     *
     * @checkstyle ParameterNameCheck (10 lines)
     * @param offlineHashFile Hash from file
     * @param offlineHash Hash by pattern
     * @param tag The Git hash to pull objects from
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public ChResolve(final Path offlineHashFile, final String offlineHash, final String tag) {
        if (offlineHashFile == null && offlineHash == null) {
            this.hash = new ChCached(new ChRemote(tag));
        } else if (offlineHash == null) {
            this.hash = new ChCached(new ChText(offlineHashFile, tag));
        } else {
            this.hash = new ChCached(new ChPattern(offlineHash, tag));
        }
    }

    @Override
    public String value() {
        return this.hash.toString();
    }

    /**
     * Getter.
     *
     * @return Resolved {@link CommitHash}
     */
    public CommitHash getCommitHash() {
        return this.hash;
    }

}
