/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collection;

/**
 * Every clue the checker knows how to follow.
 *
 * <p>The checker looks for one kind of mistake: an attribute is taken
 * from an object that certainly doesn't have it. In this program, for
 * example, {@code t} does have {@code next}, but the object attached to
 * {@code next} has no {@code foo}:</p>
 *
 * <pre> [] &gt; app
 *   inc t &gt; @
 *   [] &gt; t
 *     [] &gt; next
 *   [x] &gt; inc
 *     x.next.foo &gt; @</pre>
 *
 * <p>Three clues are planned, one per kind of object, and each fills a
 * table of its own: what an object certainly has ({@link Provides}), what
 * it must have judging by how it is used ({@link Needs}), and which types
 * are copies of which. Keeping the tables apart is what lets a smarter
 * rule add rows, or read them differently, without touching anything
 * else.</p>
 *
 * <p>Every clue reads the program as text, writing a row for an object
 * because it is <em>there</em>, not because something reaches it. The
 * alternative — starting from a root and following what actually runs —
 * gives sharper answers, since every question is then asked in a context
 * where the types are concrete, but it cannot be used here: eo-runtime is
 * a library with no entry point, so a checker that waits to be reached
 * has nothing to pull on and reports a clean bill of health for code it
 * never opened. Reading the text costs precision, which the checking loop
 * buys back later by resolving each site in its own context; reading only
 * what runs costs coverage, which nothing buys back.</p>
 *
 * <p>Then the checks are drained one by one, each of them either
 * deciding, splitting into smaller checks, or waiting for facts that may
 * never come. A mistake is reported only when the object that misses an
 * attribute is complete, so that parts of the program the checker cannot
 * see — atoms, delegation through {@code φ} — make it silent rather than
 * wrong. Only the first clue is here so far.</p>
 *
 * @since 0.67.0
 */
public final class Clues implements Clue {

    /**
     * The clues to follow, in the order they are followed.
     */
    private final Collection<Clue> all;

    /**
     * Ctor.
     */
    public Clues() {
        this(Arrays.asList(new Provides(), new Needs()));
    }

    /**
     * Ctor.
     * @param clues The clues to follow
     */
    public Clues(final Collection<Clue> clues) {
        this.all = clues;
    }

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        for (final Clue clue : this.all) {
            clue.follow(xmirs, tables);
        }
    }
}
