/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * What the names of one formation mean while its body is read.
 *
 * <p>The formation being lowered is the root scope: its voids are the
 * symbols {@code v0, v1, ...} in declaration order, and the helpers
 * bound next to its body are reachable by name. A helper that is a
 * formation of its own opens a scope inside the one it is named from
 * when it is applied: its voids are bound to the argument terms, by
 * position or by name, and its own attributes become its helpers, while
 * {@code ρ} leads back to the scope it was opened from. The parser
 * writes exactly as many {@code ρ} hops as the author did, so a name
 * from an enclosing formation is read through them and never guessed.
 * At the root, {@code ρ} leads to the object that owns the formation,
 * where only the formation itself may be named, as the call that
 * becomes a repeat.</p>
 *
 * @since 0.76.0
 */
public final class Scope {

    /**
     * The names bound to terms: the voids of the formation.
     */
    private final Map<String, Term> terms;

    /**
     * The helpers reachable by name: their {@code <o/>} elements.
     */
    private final Map<String, Xnav> helpers;

    /**
     * The name of the formation at the root, empty elsewhere.
     */
    private final String self;

    /**
     * The scope this one was opened from, none at the root.
     */
    private final List<Scope> outer;

    /**
     * Ctor, for the root.
     * @param voids The voids of the formation: names to formas, in order
     * @param name The name of the formation, or empty when the fragment
     *  is not the body of one
     * @param bound The helpers the formation binds next to its body
     */
    public Scope(final Map<String, String> voids, final String name,
        final Map<String, Xnav> bound) {
        this(Scope.symbols(voids), bound, name, Collections.emptyList());
    }

    private Scope(final Map<String, Term> values, final Map<String, Xnav> bound,
        final String name, final List<Scope> above) {
        this.terms = values;
        this.helpers = bound;
        this.self = name;
        this.outer = above;
    }

    /**
     * The term a name is bound to here.
     * @param name The name
     * @return The term, or empty when the name is not a bound void here
     */
    public Optional<Term> term(final String name) {
        return Optional.ofNullable(this.terms.get(name));
    }

    /**
     * The helper a name reaches here.
     * @param name The name
     * @return Its element, or empty when the name is not a helper here
     */
    public Optional<Xnav> helper(final String name) {
        return Optional.ofNullable(this.helpers.get(name));
    }

    /**
     * Whether this is the root, the scope of the formation being lowered.
     * @return True at the root
     */
    public boolean root() {
        return this.outer.isEmpty();
    }

    /**
     * The name of the formation being lowered.
     * @return The name, empty when the fragment is not the body of one
     */
    public String name() {
        return this.self;
    }

    /**
     * The scope this one was opened from.
     * @return The scope {@code ρ} leads to
     */
    public Scope above() {
        if (this.root()) {
            throw new IllegalStateException(
                "The reference reaches through ρ beyond the formation being lowered"
            );
        }
        return this.outer.get(0);
    }

    /**
     * The scope of a helper formation applied to arguments, inside this one.
     * @param formation The helper, an {@code <o/>} with no base
     * @param args The arguments of the application
     * @return The scope its body is read in
     */
    public Scope inside(final Xnav formation, final List<Binding> args) {
        final List<String> voids = new ArrayList<>(0);
        final Map<String, Xnav> bound = new LinkedHashMap<>();
        for (final Xnav kid : formation.elements(Filter.withName("o"))
            .collect(Collectors.toList())) {
            final String name = kid.attribute("name").text().orElse("");
            if ("∅".equals(kid.attribute("base").text().orElse(""))) {
                if (!"ρ".equals(name)) {
                    voids.add(name);
                }
            } else if (!name.isEmpty() && !"φ".equals(name)) {
                bound.put(name, kid);
            }
        }
        final Map<String, Term> values = new LinkedHashMap<>();
        for (final Binding arg : args) {
            values.put(Scope.named(voids, arg.label()), arg.value());
        }
        if (values.size() != voids.size()) {
            throw new IllegalStateException(
                String.format(
                    "The helper declares %d voids, but the application binds %d of them",
                    voids.size(), values.size()
                )
            );
        }
        return new Scope(values, bound, "", Collections.singletonList(this));
    }

    private static String named(final List<String> voids, final String label) {
        final String out;
        if (label.startsWith("α")) {
            final int idx = Integer.parseInt(label.substring(1));
            if (idx >= voids.size()) {
                throw new IllegalStateException(
                    String.format(
                        "The helper declares %d voids, but is handed an argument '%s'",
                        voids.size(), label
                    )
                );
            }
            out = voids.get(idx);
        } else if (voids.contains(label)) {
            out = label;
        } else {
            throw new IllegalStateException(
                String.format("The helper declares no void '%s' to bind", label)
            );
        }
        return out;
    }

    private static Map<String, Term> symbols(final Map<String, String> voids) {
        final Map<String, Term> out = new LinkedHashMap<>();
        int idx = 0;
        for (final Map.Entry<String, String> entry : voids.entrySet()) {
            out.put(entry.getKey(), new Symbol(String.format("v%d", idx), entry.getValue()));
            ++idx;
        }
        return out;
    }
}
