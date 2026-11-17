/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * A dotted path written in a meta directive.
 *
 * <p>Every dot introduces a segment, so a leading dot, a trailing dot and a
 * doubled dot each name a segment that is not there. Such a path is copied
 * verbatim into a base by `expand-aliases.xsl`, where it names nothing at
 * all, which is why it is caught while the line is still being read.</p>
 *
 * @since 0.64.0
 */
final class Dotted {

    /**
     * The tokens that name a scope or the root rather than an object:
     * the {@code @} decoratee, the {@code ^} parent, the {@code $} self
     * and the root, in both the spelling the source uses and the one
     * {@code LnMeta} promotes it to.
     */
    private static final Set<String> SCOPES = new HashSet<>(
        Arrays.asList("@", "^", "$", "Q", "Φ")
    );

    /**
     * The path.
     */
    private final String path;

    /**
     * Ctor.
     *
     * @param text The path, as the source wrote it
     */
    Dotted(final String text) {
        this.path = text;
    }

    /**
     * Whether the path names a segment that is not there.
     *
     * @return True if a dot of it introduces nothing
     */
    boolean broken() {
        return this.path.startsWith(".")
            || this.path.endsWith(".")
            || this.path.contains("..");
    }

    /**
     * Whether a segment of the path names a scope instead of an object.
     *
     * <p>A forma path is made of names, optionally rooted at the global
     * root. {@code expand-aliases.xsl} copies the path into a base with
     * the root in front of it, so a scope token there comes out as
     * {@code Φ.@}, which names nothing (#7926). The root itself is a
     * scope token too, and stands only at the head of a longer path.</p>
     *
     * @return True if a segment of it is a scope token
     */
    boolean scoped() {
        final String[] parts = this.path.split("\\.", -1);
        boolean found = false;
        for (int idx = 0; idx < parts.length && !found; idx = idx + 1) {
            found = Dotted.SCOPES.contains(parts[idx]) && !Dotted.rooted(parts, idx);
        }
        return found;
    }

    /**
     * Whether a segment of the path is not a {@code NAME} token.
     *
     * <p>§2.3 shapes a name as a lowercase letter and then any glyph that
     * does not end a token. {@code expand-aliases.xsl} copies the path
     * into a base, so a segment of any other shape comes out as a locator
     * that {@code XMIR.xsd} refuses, like {@code Φ.A.b} (#9316). The root
     * at the head of a longer path is a token of its own and is left to
     * {@link #scoped()}.</p>
     *
     * @return True if a segment of it is not a name
     */
    boolean unnamed() {
        final String[] parts = this.path.split("\\.", -1);
        boolean found = false;
        for (int idx = 0; idx < parts.length && !found; idx = idx + 1) {
            found = !Dotted.rooted(parts, idx) && !Dotted.names(parts[idx]);
        }
        return found;
    }

    private static boolean names(final String part) {
        boolean name = !part.isEmpty() && part.charAt(0) >= 'a' && part.charAt(0) <= 'z'
            && part.codePoints().noneMatch(code -> code == 0x1F335);
        for (int idx = 1; idx < part.length() && name; idx = idx + 1) {
            name = !Tokens.terminates(part.charAt(idx));
        }
        return name;
    }

    private static boolean rooted(final String[] parts, final int idx) {
        return idx == 0 && parts.length > 1 && "Φ".equals(parts[idx]);
    }
}
