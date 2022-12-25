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
 * @since 0.28.14
 */
public final class ChResolve implements CommitHash {

    /**
     * Read hashes from local file.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    private final Path offlineHashFile;

    /**
     * Return hash by pattern.
     * -DofflineHash=0.*.*:abc2sd3
     * -DofflineHash=0.2.7:abc2sd3,0.2.8:s4se2fe
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    private final String offlineHash;

    /**
     * The Git hash to pull objects from, in objectionary.
     */
    private final String tag;

    /**
     * Ctor.
     *
     * @param hashFile Hash from file
     * @param hash Hash by pattern
     * @param tagg The Git hash to pull objects from
     * @checkstyle ParameterNameCheck (10 lines)
     */
    public ChResolve(final Path hashFile, final String hash, final String tagg) {
        this.offlineHashFile = hashFile;
        this.offlineHash = hash;
        this.tag = tagg;
    }

    @Override
    public String value() {
        final CommitHash ret;
        if (this.offlineHashFile == null && this.offlineHash == null) {
            ret = new ChCached(new ChRemote(this.tag));
        } else if (this.offlineHash == null) {
            ret = new ChCached(new ChText(this.offlineHashFile, this.tag));
        } else {
            ret = new ChCached(new ChPattern(this.offlineHash, this.tag));
        }
        return ret.value();
    }

}
