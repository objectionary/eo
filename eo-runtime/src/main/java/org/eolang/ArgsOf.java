/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2020 Yegor Bugayenko
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

package org.eolang;

import java.util.HashMap;
import java.util.Map;

/**
 * Arguments of an object.
 *
 * @since 0.1
 */
public final class ArgsOf implements Args {

    /**
     * The map.
     */
    private final Map<String, Object> map;

    /**
     * Ctor.
     */
    public ArgsOf() {
        this(new HashMap<>(0));
    }

    /**
     * Ctor.
     * @param entries Entries
     */
    public ArgsOf(final Entry... entries) {
        this(new ArgsOf(), entries);
    }

    /**
     * Ctor.
     * @param before Args before
     * @param entries Entries
     */
    public ArgsOf(final Args before, final Entry... entries) {
        this(ArgsOf.combine(before, entries));
    }

    /**
     * Ctor.
     * @param args The args
     */
    private ArgsOf(final Map<String, Object> args) {
        this.map = args;
    }

    @Override
    public Object get(final String key) {
        return this.map.get(key);
    }

    @Override
    public boolean has(final String key) {
        return this.map.containsKey(key);
    }

    @Override
    public Iterable<String> keys() {
        return this.map.keySet();
    }

    /**
     * Combine them.
     * @param before Map before
     * @param entries Entries
     * @return New map
     */
    private static Map<String, Object> combine(
        final Args before, final Entry... entries) {
        final Map<String, Object> after = new HashMap<>(entries.length);
        for (final String key : before.keys()) {
            after.put(key, before.get(key));
        }
        for (final Entry entry : entries) {
            after.put(entry.key(), entry.value());
        }
        return after;
    }

}
