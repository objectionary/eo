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

package org.eolang;

import java.util.logging.Logger;

/**
 * An attribute that logs all its operations to the console (very
 * convenient for debugging).
 *
 * @since 0.24
 */
@Versionized
final class AtLogged implements Attr {

    /**
     * Origin.
     */
    private final Attr origin;

    /**
     * Owner.
     */
    private final String owner;

    /**
     * Logger.
     */
    private final Logger log;

    /**
     * Ctor.
     * @param attr Attribute
     * @param label Label
     */
    AtLogged(final Attr attr, final String label) {
        this(attr, label, Logger.getLogger(AtLogged.class.getName()));
    }

    /**
     * Ctor.
     * @param attr Attribute
     * @param label Label
     * @param logger Logger
     */
    AtLogged(final Attr attr, final String label, final Logger logger) {
        this.origin = attr;
        this.owner = label;
        this.log = logger;
    }

    @Override
    public String toString() {
        return this.origin.toString();
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }

    @Override
    public Attr copy(final Phi self) {
        this.log.info(String.format("  %s.copy()...\n", this.owner));
        final Attr ret = this.origin.copy(self);
        this.log.info(String.format("  %s.copy()!\n", this.owner));
        return ret;
    }

    @Override
    public Phi get() {
        this.log.info(String.format("  %s.get()...\n", this.owner));
        final Phi ret = this.origin.get();
        this.log.info(String.format("  %s.get()! -> %d\n", this.owner, ret.hashCode()));
        return ret;
    }

    @Override
    public void put(final Phi src) {
        this.log.info(String.format("  %s.put()...\n", this.owner));
        this.origin.put(src);
        this.log.info(String.format("  %s.put()!\n", this.owner));
    }
}
