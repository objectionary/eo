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
import java.util.Random;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
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
     * Enter last object.
     */
    void enter();

    /**
     * Leave current object.
     */
    void leave();

    /**
     * Mark next object for aliasing.
     */
    void alias();

    void closeAlias();

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
        private final Deque<String> aliases = new LinkedList<>();

        @Override
        public void start(final int line, final int pos) {
            this.dirs.add("o");
            this.prop("line", line);
            this.prop("pos", pos);
            if (!this.aliases.isEmpty()) {
                final String alias = String.join("-", this.aliases);
                this.prop("alias", alias);
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
        public void enter() {
            this.dirs.xpath("o[last()]").strict(1);
        }

        @Override
        public void leave() {
            this.dirs.up();
        }

        private final AtomicInteger next = new AtomicInteger(1);
        private final Random random = new Random();


        @Override
        public void alias() {
            this.aliases.push(
                String.format(
                    "%s-scope-%s",
                    Math.abs(this.random.nextInt(100)),
                    this.next.getAndIncrement()
                )
            );
        }

        @Override
        public void closeAlias() {
            this.aliases.remove();
            this.next.decrementAndGet();
        }

        @Override
        public Iterator<Directive> iterator() {
            return this.dirs.iterator();
        }
    }
}
