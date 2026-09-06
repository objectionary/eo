/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 * A meta-directive line — §3.2 of the spec.
 *
 * <p>Form: {@code +name [parts]}. The name follows the leading {@code +}
 * with no space; parts are space-separated tokens that follow. Legal only
 * at indent 0 (R-3.2.1) and only before any non-meta object has been
 * emitted (R-3.2.2). At most one space between parts (R-3.2.4).</p>
 *
 * <p>A leading {@code Q} in any part is promoted to {@code Φ} in the
 * emitted XMIR (R-3.2.3 / R-9.3). This class does the promotion at
 * emission time.</p>
 *
 * <p>Two directives name what they act on and are meaningless without
 * it: {@code +package} takes exactly one argument, and {@code +alias}
 * at least one — the shorthand {@code +alias Φ.foo} or the full
 * {@code +alias foo Φ.bar}. A bare one of either is rejected here, so
 * that {@code expand-aliases} is never handed an alias with nothing to
 * expand.</p>
 *
 * <p>{@code Q} names the global root and nothing else, so a
 * {@code +alias} that would give the token another meaning
 * ({@code +alias Q Q.foo}, or the shorthand {@code +alias Q}) is
 * rejected here — {@code expand-aliases} reads that first part as the
 * alias name and would otherwise let one file rewrite the root.</p>
 *
 * @since 0.1
 */
final class LnMeta implements Line {

    /**
     * The global root, what a {@code Q} part is promoted to (R-9.3).
     */
    private static final String ROOT = "Φ";

    /**
     * The meta line's span.
     */
    private final Span span;

    /**
     * Ctor.
     * @param source The meta span
     */
    LnMeta(final Span source) {
        this.span = source;
    }

    @Override
    public void into(final Stack stack, final Globals globals, final Emit emit) {
        if (this.span.indent() != 0) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                String.format(
                    Locale.ROOT,
                    "meta directive must sit at indent 0, found indent %d",
                    this.span.indent()
                )
            );
        }
        if (globals.firstObjectEmitted()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "meta directive must precede all other objects"
            );
        }
        if (globals.inMetaHeader() && globals.pendingBlanks() > 0) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "blank line between meta directives is forbidden (R-6.5.7); the meta header is a single contiguous block"
            );
        }
        if (!globals.inMetaHeader() && globals.pendingBlanks() > 0
            && globals.pendingComments().isEmpty()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "meta header must appear at the top of the file"
            );
        }
        final String body = this.span.body();
        final int space = body.indexOf(' ');
        final String head;
        final List<String> parts;
        if (space < 0) {
            head = body.substring(1);
            parts = new ArrayList<>(0);
        } else {
            head = body.substring(1, space);
            parts = this.split(body.substring(space + 1), space + 1);
        }
        this.checkHead(head, parts);
        globals.seal(emit, this.span);
        globals.markMeta();
        globals.clearBlanks();
        emit.meta(this.span.line(), head, parts);
    }

    private void checkHead(final String head, final List<String> parts) {
        if (head.isEmpty()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "meta directive requires a name"
            );
        }
        if (!head.matches("[a-z][a-z0-9]*")) {
            throw new ParseError(
                this.span.line(), this.span.indent() + 1,
                "meta name must be lowercase letters and digits, starting with a letter"
            );
        }
        if ("package".equals(head)) {
            this.checkPackage(parts);
        }
        if ("alias".equals(head)) {
            this.checkAlias(parts);
        }
    }

    private void checkPackage(final List<String> parts) {
        if (parts.size() != 1) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "'+package' directive requires exactly one argument"
            );
        }
        if (new Dotted(parts.get(0)).broken()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "'+package' path must not have an empty segment"
            );
        }
    }

    private void checkAlias(final List<String> parts) {
        if (parts.isEmpty()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "'+alias' directive requires at least one argument"
            );
        }
        if (LnMeta.ROOT.equals(parts.get(0))) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "'+alias' cannot rename the root token Q"
            );
        }
        final Dotted target = new Dotted(parts.get(parts.size() - 1));
        if (target.broken()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "'+alias' target must not have an empty segment"
            );
        }
        if (target.scoped()) {
            throw new ParseError(
                this.span.line(), this.span.indent(),
                "'+alias' target must be an object name, not a scope token"
            );
        }
    }

    private List<String> split(final String tail, final int base) {
        final List<String> out = new ArrayList<>(2);
        int idx = 0;
        while (idx < tail.length()) {
            int end = idx;
            while (end < tail.length() && !Character.isWhitespace(tail.charAt(end))) {
                end = end + 1;
            }
            if (end == idx || end < tail.length() && tail.charAt(end) != ' ') {
                throw new ParseError(
                    this.span.line(), this.span.indent() + base + end,
                    "meta parts must be separated by a single ASCII space"
                );
            }
            final String part = tail.substring(idx, end);
            final int control = new Scrubbed(part).found();
            if (control >= 0) {
                throw new ParseError(
                    this.span.line(), this.span.indent() + base + idx + control,
                    "control character is not allowed in a meta"
                );
            }
            out.add(LnMeta.promoteQ(part));
            idx = end;
            if (idx < tail.length()) {
                idx = idx + 1;
            }
        }
        return out;
    }

    private static String promoteQ(final String part) {
        final String promoted;
        if ("Q".equals(part)) {
            promoted = LnMeta.ROOT;
        } else if (part.startsWith("Q.")) {
            promoted = LnMeta.ROOT.concat(part.substring(1));
        } else {
            promoted = part;
        }
        return promoted;
    }
}
