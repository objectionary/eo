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
package org.eolang.maven.rust;

import java.io.IOException;
import org.eolang.maven.footprint.Footprint;

/**
 * Created to be saved.
 * @since 0.30
 * @checkstyle VisibilityModifierCheck (20 lines)
 */
public abstract class Savable {

    /**
     * Name of file.
     */
    protected final String name;

    /**
     * Extension of the file, i.e. "rs".
     */
    protected final String ext;

    /**
     * Ctor.
     * @param name Name of the file.
     * @param ext Extension.
     */
    public Savable(final String name, final String ext) {
        this.name = name;
        this.ext = ext;
    }

    /**
     * Save it by footprint.
     * @param footprint Footprint.
     * @throws IOException If any issues with IO.
     */
    public void save(final Footprint footprint) throws IOException {
        footprint.save(this.name, this.ext, this::content);
    }

    /**
     * Content inside file.
     * @return Content.
     */
    abstract String content();
}
