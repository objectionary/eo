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
package org.eolang.parser;

import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.UUID;
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
     */
    void start(int line, int pos);

    /**
     * Add data.
     * @param data Data.
     */
    void data(String data);

    /**
     * Property.
     * @param key Key.
     * @param value Value.
     */
    void prop(String key, Object value);

    /**
     * Empty propery.
     * @param key Key.
     */
    void prop(String key);

    /**
     * Change property by given xpath.
     * @param key Key.
     * @param xpath Xpath.
     */
    void xprop(String key, Object xpath);

    /**
     * Enter last object.
     */
    void enter();

    /**
     * Leave current object.
     */
    void leave();

    /**
     * Mark the next object for scoping.
     */
    void scope();

    /**
     * Mark the current object as last inside the scope.
     * Last object that relates to the scope.
     */
    void closeScope();

    /**
     * Xembly object tree.
     * @since 0.1
     */
    final class ObjXembly implements Objects {

        /**
         * Collected directives.
         */
        private final Directives dirs = new Directives();

        /**
         * Generated aliases.
         */
        private final Deque<String> scopes = new LinkedList<>();

        @Override
        public void start(final int line, final int pos) {
            this.dirs.add("o");
            this.prop("line", line);
            this.prop("pos", pos);
            if (!this.scopes.isEmpty()) {
                this.prop("scope", String.join("-", this.scopes));
            }
        }

        @Override
        public void data(final String data) {
            this.dirs.set(data);
        }

        @Override
        public void prop(final String key, final Object type) {
            this.dirs.attr(key, type);
        }

        @Override
        public void prop(final String key) {
            this.prop(key, "");
        }

        @Override
        public void xprop(final String key, final Object xpath) {
            this.dirs.xattr(key, xpath);
        }

        @Override
        public void enter() {
            this.dirs.xpath("o[last()]").strict(1);
        }

        @Override
        public void leave() {
            this.dirs.up();
        }

        @Override
        public void scope() {
            this.scopes.push(
                String.format(
                    "scope-%s",
                    UUID.randomUUID()
                )
            );
        }

        @Override
        public void closeScope() {
            this.scopes.remove();
        }

        @Override
        public Iterator<Directive> iterator() {
            return this.dirs.iterator();
        }
    }
}
