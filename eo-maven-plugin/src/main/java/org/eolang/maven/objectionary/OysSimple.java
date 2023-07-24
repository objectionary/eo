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
package org.eolang.maven.objectionary;

import java.util.HashMap;
import java.util.Map;

/**
 * Simple objectionaries.
 * @since 0.26.6
 */
public final class OysSimple implements Objectionaries {
    /**
     * Hash-Objectionary map.
     * @checkstyle MemberNameCheck (5 lines)
     */
    private final Map<String, Objectionary> objectionaries;

    /**
     * Ctor.
     */
    public OysSimple() {
        this(new HashMap<>());
    }

    /**
     * Ctor.
     * @param map Hash-map.
     */
    OysSimple(final Map<String, Objectionary> map) {
        this.objectionaries = map;
    }

    @Override
    public OysSimple with(final String hash, final Objectionary objectionary) {
        this.objectionaries.putIfAbsent(hash, objectionary);
        return this;
    }

    @Override
    public boolean has(final String hash) {
        return this.objectionaries.containsKey(hash);
    }

    @Override
    public Objectionary get(final String hash) {
        return this.objectionaries.get(hash);
    }
}
