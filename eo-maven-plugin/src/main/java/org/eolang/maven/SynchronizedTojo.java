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
package org.eolang.maven;

import com.yegor256.tojos.Tojo;

/**
 * Thread-safe version.xsl of tojo. Synchronizes on global single lock within JVM.
 * @since 1.0
 * @todo #1230:30min Replace this custom {@link SynchronizedTojo} with
 *  implementation from Tojo framework once completed in
 *  <a href="https://github.com/yegor256/tojos/issues/16">#16</a>. This class
 *  should be removed and all usages updated accordingly.
 */
final class SynchronizedTojo implements Tojo {
    /**
     * Lock object.
     */
    private static final Object LOCK = SynchronizedTojo.class;

    /**
     * Origin tojo.
     */
    private final Tojo origin;

    /**
     * Ctor.
     * @param origin Tojo
     */
    SynchronizedTojo(final Tojo origin) {
        this.origin = origin;
    }

    @Override
    public boolean exists(final String key) {
        synchronized (SynchronizedTojo.LOCK) {
            return this.origin.exists(key);
        }
    }

    @Override
    public String get(final String key) {
        synchronized (SynchronizedTojo.LOCK) {
            return this.origin.get(key);
        }
    }

    @Override
    public Tojo set(final String key, final Object value) {
        synchronized (SynchronizedTojo.LOCK) {
            return this.origin.set(key, value);
        }
    }
}
