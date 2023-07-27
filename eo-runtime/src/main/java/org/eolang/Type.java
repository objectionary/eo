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
package org.eolang;

import EOorg.EOeolang.*;
import org.cactoos.map.MapEntry;
import org.cactoos.map.MapOf;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Type of objects.
 *
 * @since 0.29.6
 */
public class Type {
    /**
     * Map of default data types.
     */
    private static final Map<String, Integer> DATA = new MapOf<>(
        new MapEntry<>(EObool.class.toString(), 1),
        new MapEntry<>(EOfloat.class.toString(), 2),
        new MapEntry<>(EOint.class.toString(), 3),
        new MapEntry<>(EOstring.class.toString(), 4),
        new MapEntry<>(EObytes.class.toString(), 5)
    );

    /**
     * Type.
     */
    private final AtomicInteger type = new AtomicInteger(5);

    /**
     * Get best type for given class.
     * @param phi Phi.
     * @return Type for given class.
     */
    public int best(final Phi phi) {
        final String clazz = phi.getClass().toString();
        final int tpe;
        if (DATA.containsKey(clazz)) {
            tpe = DATA.get(clazz);
        } else {
            tpe = this.next();
        }
        return tpe;
    }

    /**
     * Next type.
     * @return Type.
     */
    public int next() {
        return this.type.addAndGet(1);
    }
}
