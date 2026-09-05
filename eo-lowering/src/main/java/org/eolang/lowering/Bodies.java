/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * The bodies of one formation, reduced as the repeats between them are
 * found.
 *
 * <p>The formation's own body goes first, over the symbols of its
 * voids, with the recursive helpers the {@link Cycles} name marked in
 * its {@link Scope} as bodies to resume rather than read. Every repeat
 * that settles a tree into a helper declares that helper's voids in the
 * {@link Minted} ledger, with the formas the repeat hands over, and the
 * helper's body is then reduced over the symbols of those voids, in a
 * scope of its own inside the root, which may declare further bodies,
 * until every declared body is reduced. A program whose bodies only
 * resume one another never answers and is refused here, so that the
 * caller treats it like any other fragment that stays as written.</p>
 *
 * @since 0.76.0
 */
public final class Bodies {

    /**
     * The reduction settling each tree.
     */
    private final Reduction core;

    /**
     * The XMIR fragment that is the formation's own body.
     */
    private final Xnav fragment;

    /**
     * The voids of the formation: names to formas, in declaration order.
     */
    private final Map<String, String> voids;

    /**
     * The name of the formation, or empty.
     */
    private final String formation;

    /**
     * The helpers the formation binds next to its body.
     */
    private final Map<String, Xnav> helpers;

    /**
     * Ctor.
     * @param reduction The reduction settling each tree
     * @param xmir The XMIR fragment that is the formation's own body
     * @param inputs The voids of the formation: names to formas, in order
     * @param name The name of the formation, or empty
     * @param bound The helpers the formation binds next to its body
     */
    public Bodies(final Reduction reduction, final Xnav xmir,
        final Map<String, String> inputs, final String name, final Map<String, Xnav> bound) {
        this.core = reduction;
        this.fragment = xmir;
        this.voids = inputs;
        this.formation = name;
        this.helpers = bound;
    }

    /**
     * The program.
     * @return The bodies, the formation's own first
     * @throws IOException If the binary cannot be run
     */
    public Program program() throws IOException {
        final Minted minted = new Minted(this.voids);
        final Scope root = new Scope(
            this.voids, this.formation, this.helpers, new Cycles(this.helpers).names()
        );
        final List<Body> out = new ArrayList<>(1);
        out.add(
            new Body(
                "", 0, new ArrayList<>(this.voids.values()),
                this.core.settled(
                    new Parsed(this.fragment, root, Collections.emptyList()).term(), minted
                )
            )
        );
        final Set<String> done = new HashSet<>();
        done.add("");
        Optional<String> next = Bodies.pending(minted, done);
        while (next.isPresent()) {
            final String name = next.get();
            done.add(name);
            final Xnav helper = this.helpers.get(name);
            out.add(
                new Body(
                    name, minted.offset(name), minted.voids(name),
                    this.core.settled(
                        new Parsed(
                            Bodies.body(name, helper),
                            root.body(helper, minted.offset(name), minted.voids(name)),
                            Collections.singletonList(name)
                        ).term(),
                        minted
                    )
                )
            );
            next = Bodies.pending(minted, done);
        }
        final Program program = new Program(out, this.voids);
        program.carrier();
        return program;
    }

    private static Optional<String> pending(final Minted minted, final Set<String> done) {
        return minted.names().stream().filter(name -> !done.contains(name)).findFirst();
    }

    private static Xnav body(final String name, final Xnav helper) {
        final Optional<Xnav> found = helper.elements(Filter.withName("o"))
            .filter(kid -> "φ".equals(kid.attribute("name").text().orElse("")))
            .filter(kid -> kid.attribute("base").text().isPresent())
            .findFirst();
        if (!found.isPresent()) {
            throw new IllegalStateException(
                String.format("The helper 'ξ.%s' has no body to resume", name)
            );
        }
        return found.get();
    }
}
