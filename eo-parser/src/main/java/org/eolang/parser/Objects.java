/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
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
     * Empty property.
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
        public Iterator<Directive> iterator() {
            return this.dirs.iterator();
        }
    }
}
