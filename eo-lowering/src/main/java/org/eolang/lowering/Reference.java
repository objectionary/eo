/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * One name a fragment reaches in a scope, with the arguments handed to it.
 *
 * <p>The name is a void of the scope, which stands as its symbol and
 * takes no arguments, or a helper of the scope, which is read where it
 * is named: an application over the voids of the scope stands as its
 * own body, and a formation of its own is applied, its body read in the
 * {@link Scope} that binds its voids to the arguments. A helper the
 * scope knows to be recursive is not read but resumed: naming it is the
 * {@link Again} of that helper's body, over the arguments, and the
 * reduction turns it into a repeat when it stands in a tail position.
 * Any other helper that names itself, directly or through another
 * helper, is a cycle and is refused, since reading it would never
 * end.</p>
 *
 * @since 0.76.0
 */
public final class Reference {

    /**
     * The scope the name is reached in.
     */
    private final Scope scope;

    /**
     * The helpers being read at the moment, outermost first.
     */
    private final Collection<String> trail;

    /**
     * The name.
     */
    private final String name;

    /**
     * The arguments handed to the name.
     */
    private final List<Binding> args;

    /**
     * Ctor.
     * @param where The scope the name is reached in
     * @param above The helpers being read at the moment, outermost first
     * @param label The name
     * @param arguments The arguments handed to the name
     */
    public Reference(final Scope where, final Collection<String> above,
        final String label, final List<Binding> arguments) {
        this.scope = where;
        this.trail = above;
        this.name = label;
        this.args = arguments;
    }

    /**
     * The term the name stands for.
     * @return The term
     */
    public Term term() {
        final Optional<Term> term = this.scope.term(this.name);
        final Optional<Xnav> helper = this.scope.helper(this.name);
        final Term out;
        if (term.isPresent()) {
            if (!this.args.isEmpty()) {
                throw new IllegalStateException(
                    String.format("The reference 'ξ.%s' cannot take arguments", this.name)
                );
            }
            out = term.get();
        } else if (helper.isPresent()) {
            out = this.applied(helper.get());
        } else {
            throw new IllegalStateException(
                String.format(
                    "The reference 'ξ.%s' names no void or helper of the fragment", this.name
                )
            );
        }
        return out;
    }

    private Term applied(final Xnav helper) {
        final Term out;
        if (this.scope.resumes(this.name)) {
            out = new Again(
                this.name,
                this.args.stream().map(Binding::value).collect(Collectors.toList())
            );
        } else {
            out = this.read(helper);
        }
        return out;
    }

    private Term read(final Xnav helper) {
        if (this.trail.contains(this.name)) {
            throw new IllegalStateException(
                String.format(
                    "The helper 'ξ.%s' reads itself, so the fragment never settles", this.name
                )
            );
        }
        final Collection<String> deeper = new LinkedHashSet<>(this.trail);
        deeper.add(this.name);
        final Term out;
        if (helper.attribute("base").text().isPresent()) {
            if (!this.args.isEmpty()) {
                throw new IllegalStateException(
                    String.format(
                        "The helper 'ξ.%s' is an application and cannot take arguments",
                        this.name
                    )
                );
            }
            out = new Parsed(helper, this.scope, deeper).term();
        } else {
            out = new Parsed(
                this.body(helper), this.scope.inside(helper, this.args), deeper
            ).term();
        }
        return out;
    }

    private Xnav body(final Xnav helper) {
        final Optional<Xnav> found = helper.elements(Filter.withName("o"))
            .filter(kid -> "φ".equals(kid.attribute("name").text().orElse("")))
            .filter(kid -> kid.attribute("base").text().isPresent())
            .findFirst();
        if (!found.isPresent()) {
            throw new IllegalStateException(
                String.format("The helper 'ξ.%s' has no body to apply", this.name)
            );
        }
        return found.get();
    }
}
