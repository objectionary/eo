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
 * <p>A clue reads a program and writes down one kind of fact about the
 * objects in it. There is a clue per kind of object, and each fills a table
 * of its own: what an object certainly has ({@link Provides}), what it must
 * have judging by how it is used ({@link Needs}), and which types are copies
 * of which ({@link Links}). Keeping the tables apart is what lets a smarter
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
 * <p>No clue decides anything, which is why they can be read one after
 * another in any order, and why nothing here says whether a program is
 * wrong. Judging used to live beside these rules and was taken out again
 * in #6661: it reported nothing, because a verdict needs the object that
 * misses an attribute to have been seen whole, and almost none of them have
 * been. It comes back when every object can be given a type, rather than
 * only those a call site happens to reach.</p>
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
        this(Arrays.asList(new Provides(), new Needs(), new Links()));
    }

    /**
     * Ctor.
     *
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
