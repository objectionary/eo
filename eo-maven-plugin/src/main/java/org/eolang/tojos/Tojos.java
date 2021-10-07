/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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
package org.eolang.tojos;

import java.io.IOException;
import java.util.Collection;
import org.cactoos.Func;

/**
 * Text Object Java Object (TOJO) in a storage.
 *
 * @since 0.12
 */
public interface Tojos {

    /**
     * How many out there?
     *
     * @return Total count
     * @throws IOException If fails
     */
    int size() throws IOException;

    /**
     * Add new object and raise exception if there are version
     * conflicts.
     *
     * @param name The name of the object
     * @throws IOException If fails
     */
    Tojo add(String name) throws IOException;

    /**
     * Select some fobjects.
     *
     * @param filter The filter
     * @return Collection of them
     * @throws IOException If fails
     */
    Collection<Tojo> select(Func<Tojo, Boolean> filter) throws IOException;

}
