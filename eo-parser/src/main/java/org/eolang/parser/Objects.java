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
package org.eolang.parser;

import java.util.Iterator;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * Object tree.
 * @since 0.1
 */
interface Objects extends Iterable<Directive> {

    /**
     * Start new object.
     * @param line At line.
     * @param pos At position.
     * @return Self.
     */
    Objects start(int line, int pos);

    /**
     * Start new object without position.
     * @return Self.
     */
    Objects start();

    /**
     * Add data.
     * @param data Data.
     * @return Self.
     */
    Objects data(String data);

    /**
     * Property.
     * @param key Key.
     * @param value Value.
     * @return Self.
     */
    Objects prop(String key, Object value);

    /**
     * Empty propery.
     * @param key Key.
     * @return Self.
     */
    Objects prop(String key);

    /**
     * Change property by given xpath.
     * @param key Key.
     * @param xpath Xpath.
     * @return Self.
     */
    Objects xprop(String key, Object xpath);

    /**
     * Enter last object.
     * @return Self.
     */
    Objects enter();

    /**
     * Leave current object.
     * @return Self.
     */
    Objects leave();

    /**
     * Remove current object.
     * @return Self.
     */
    Objects remove();

    /**
     * Xembly object tree.
     * @since 0.1
     */
    final class ObjXembly implements Objects {

        /**
         * Collected directives.
         */
        private final Directives dirs = new Directives();

        @Override
        public Objects start(final int line, final int pos) {
            this.dirs.add("o");
            return this.prop("line", line).prop("pos", pos);
        }

        @Override
        public Objects start() {
            this.dirs.add("o");
            return this;
        }

        @Override
        public Objects data(final String data) {
            this.dirs.set(data);
            return this;
        }

        @Override
        public Objects prop(final String key, final Object type) {
            this.dirs.attr(key, type);
            return this;
        }

        @Override
        public Objects prop(final String key) {
            return this.prop(key, "");
        }

        @Override
        public Objects xprop(final String key, final Object xpath) {
            this.dirs.xattr(key, xpath);
            return this;
        }

        @Override
        public Objects enter() {
            this.dirs.xpath("o[last()]").strict(1);
            return this;
        }

        @Override
        public Objects leave() {
            this.dirs.up();
            return this;
        }

        @Override
        public Objects remove() {
            this.dirs.remove();
            return this;
        }

        @Override
        public Iterator<Directive> iterator() {
            return this.dirs.iterator();
        }
    }
}
