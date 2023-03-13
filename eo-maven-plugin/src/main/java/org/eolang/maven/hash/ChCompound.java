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
 * @todo #1569:30min Now, this class is a configurable. This is not
 *  a good practice. More technically, encapsulated properties must not
 *  be used to change the behavior of an object.
 *  Need to use composable decorators to make this class
 *  not configurable.
 */
public final class ChCompound implements CommitHash {

    /**
     * Read hashes from local file.
     */
    private final Path file;

    /**
     * Return hash by pattern.
     * -DofflineHash=0.*.*:abc2sd3
     * -DofflineHash=0.2.7:abc2sd3,0.2.8:s4se2fe
     */
    private final String hash;

    /**
     * The Git hash to pull objects from, in objectionary.
     */
    private final String tag;

    /**
     * Ctor.
     *
     * @param data Hash from file
     * @param text Hash by pattern
     * @param label The Git hash to pull objects from
     */
    public ChCompound(final Path data, final String text, final String label) {
        this.file = data;
        this.hash = text;
        this.tag = label;
    }

    @Override
    public String value() {
        final CommitHash ret;
        if (this.file == null && this.hash == null) {
            ret = new ChRemote(this.tag);
        } else if (this.hash == null) {
            ret = new ChText(this.file, this.tag);
        } else {
            ret = new ChPattern(this.hash, this.tag);
        }
        return new ChCached(ret).value();
    }

}
