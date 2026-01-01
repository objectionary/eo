/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Iterator;
import org.antlr.v4.runtime.ParserRuleContext;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * Object tree.
 * @since 0.1
 */
final class Objects implements Iterable<Directive> {

    /**
     * Collected directives.
     */
    private final Directives dirs = new Directives();

    @Override
    public Iterator<Directive> iterator() {
        return this.dirs.iterator();
    }

    /**
     * Start new object.
     * @param ctx Context
     * @return Self.
     */
    Objects start(final ParserRuleContext ctx) {
        return this.start(ctx.getStart().getLine(), ctx.getStart().getCharPositionInLine());
    }

    /**
     * Start new object.
     * @param line At line.
     * @param pos At position.
     * @return Self.
     */
    Objects start(final int line, final int pos) {
        this.dirs.add("o");
        return this.prop("line", line).prop("pos", pos);
    }

    /**
     * Add data.
     * @param data Data.
     * @return Self.
     */
    Objects data(final String data) {
        this.dirs.set(data);
        return this;
    }

    /**
     * Property.
     * @param key Key.
     * @param type Type.
     * @return Self.
     */
    Objects prop(final String key, final Object type) {
        this.dirs.attr(key, type);
        return this;
    }

    /**
     * Empty property.
     * @param key Key.
     * @return Self.
     */
    Objects prop(final String key) {
        return this.prop(key, "");
    }

    /**
     * Change property by given xpath.
     * @param key Key.
     * @param xpath Xpath.
     * @return Self.
     */
    Objects xprop(final String key, final Object xpath) {
        this.dirs.xattr(key, xpath);
        return this;
    }

    /**
     * Enter last object.
     * @return Self.
     */
    Objects enter() {
        this.dirs.xpath("o[last()]").strict(1);
        return this;
    }

    /**
     * Leave current object.
     * @return Self.
     */
    Objects leave() {
        this.dirs.up();
        return this;
    }
}
