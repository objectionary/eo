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

/**
 * One tojo.
 *
 * @since 0.12
 */
public interface Tojo {

    /**
     * This attribute exists.
     *
     * @param key The name of the attribute
     * @return TRUE if exists
     * @throws IOException If fails
     */
    boolean exists(String key) throws IOException;

    /**
     * Get attribute.
     *
     * @param key The name of the attribute
     * @return The value
     * @throws IOException If fails
     */
    String get(String key) throws IOException;

    /**
     * Set attribute.
     *
     * @param key The name of the attribute
     * @param value The value
     * @return Itself
     * @throws IOException If fails
     */
    Tojo set(String key, String value) throws IOException;

}
