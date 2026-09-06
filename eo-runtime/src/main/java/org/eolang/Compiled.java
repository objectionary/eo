/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Collections;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * A regular expression built from its source once, for every caller that
 * asks for the same source afterwards.
 *
 * <p>Building a regular expression costs more than matching with it, and
 * the two atoms of {@code string.regex} read their source afresh on every
 * run: {@code pattern.checked} to learn whether it builds at all and
 * {@code pattern.match.searched} to search with it, once for every block a
 * walk over the text finds. A walk over ten blocks therefore paid for
 * eleven builds of one expression. Keeping the built patterns here, under
 * the source they came from, leaves one build per source.</p>
 *
 * <p>The patterns are shared by every caller in the process, and the one
 * asked for longest ago goes first once there are too many of them, so a
 * program that keeps making new expressions does not fill the heap with
 * them. A source the engine chokes on is not kept, so the next caller hears
 * the same refusal.</p>
 *
 * @since 0.77
 */
final class Compiled {

    /**
     * The patterns built so far, under the sources they came from.
     */
    private static final Map<String, Pattern> PATTERNS = Collections.synchronizedMap(
        new Lru<>(256)
    );

    /**
     * The source of the expression.
     */
    private final String source;

    /**
     * Ctor.
     * @param src The source of the expression, as the engine reads it
     */
    Compiled(final String src) {
        this.source = src;
    }

    /**
     * Return it.
     * @return The pattern built from the source
     */
    Pattern it() {
        return Compiled.PATTERNS.computeIfAbsent(this.source, Pattern::compile);
    }
}
