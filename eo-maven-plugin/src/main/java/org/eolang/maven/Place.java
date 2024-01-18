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
package org.eolang.maven;

import java.io.File;
import java.nio.file.Path;
import org.eolang.maven.name.DelimitedName;
import org.eolang.maven.name.ObjectName;
import org.eolang.maven.util.JoinedUnderscore;

/**
 * Make the place for the object.
 *
 * @since 0.1
 */
public final class Place {

    /**
     * The name of the object, e.g. "org.eolang.io.stdout"
     */
    private final DelimitedName name;

    /**
     * Ctor.
     * @param obj The name of the object
     */
    public Place(final ObjectName obj) {
        this(obj.toString());
    }

    /**
     * Ctor.
     * @param obj The name of the object
     */
    public Place(final String obj) {
        this.name = new DelimitedName(obj);
    }

    /**
     * Make a full path.
     * @param dir The dir
     * @param ext The ext
     * @return Full path
     */
    public Path make(final Path dir, final String ext) {
        final StringBuilder out = new StringBuilder();
        out.append(this.name.title().replace(".", File.separator));
        this.name.label().ifPresent(
            version -> {
                out.append(new JoinedUnderscore("", version).asString());
            });
        if (!ext.isEmpty()) {
            out.append('.').append(ext);
        }
        return dir.resolve(out.toString());
    }
}
