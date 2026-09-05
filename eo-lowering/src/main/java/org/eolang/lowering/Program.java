/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * The bodies one formation reduces to, the formation's own first.
 *
 * <p>A formation whose helpers apply themselves or each other in tail
 * positions is one loop with a state: which body runs next. Every body
 * is reduced once, over the symbols of its own voids, and every path of
 * it answers or resumes a body with the values that body's voids take
 * next, so the whole program is a state machine over the union of the
 * voids, and the Java of it is one {@code while (true)} that runs the
 * body the state names. A formation that never resumes anything is a
 * program of one body, the plain case. The program answers one forma,
 * whatever body the answer comes from, and a program whose bodies only
 * resume one another answers nothing and is refused.</p>
 *
 * @since 0.76.0
 */
public final class Program {

    /**
     * The bodies, the formation's own first.
     */
    private final List<Body> parts;

    /**
     * The voids of the formation: names to formas, in declaration order.
     */
    private final Map<String, String> voids;

    /**
     * Ctor.
     * @param bodies The bodies, the formation's own first
     * @param inputs The voids of the formation: names to formas, in order
     */
    public Program(final List<Body> bodies, final Map<String, String> inputs) {
        this.parts = bodies;
        this.voids = inputs;
    }

    /**
     * The bodies.
     * @return The bodies, the formation's own first
     */
    public List<Body> bodies() {
        return Collections.unmodifiableList(this.parts);
    }

    /**
     * The voids of the formation.
     * @return Names to formas, in declaration order
     */
    public Map<String, String> inputs() {
        return Collections.unmodifiableMap(this.voids);
    }

    /**
     * The formas of all the voids of all the bodies.
     * @return The formas, by the positions the bodies know
     */
    public List<String> formas() {
        final List<String> out = new ArrayList<>(
            Collections.nCopies(
                this.parts.stream()
                    .mapToInt(body -> body.offset() + body.formas().size())
                    .max().orElse(0),
                ""
            )
        );
        for (final Body body : this.parts) {
            for (int idx = 0; idx < body.formas().size(); ++idx) {
                out.set(body.offset() + idx, body.formas().get(idx));
            }
        }
        return out;
    }

    /**
     * The body of a name.
     * @param name The name of the helper, empty for the formation itself
     * @return The body
     */
    public Body body(final String name) {
        return this.parts.get(this.index(name));
    }

    /**
     * The position of the body of a name, which is the state that runs it.
     * @param name The name of the helper, empty for the formation itself
     * @return The position, zero for the formation itself
     */
    public int index(final String name) {
        for (int idx = 0; idx < this.parts.size(); ++idx) {
            if (this.parts.get(idx).name().equals(name)) {
                return idx;
            }
        }
        throw new IllegalStateException(
            String.format("The program has no body named '%s'", name)
        );
    }

    /**
     * The forma the program answers.
     * @return The forma, the same from every body that answers
     */
    public String carrier() {
        final List<String> formas = this.parts.stream()
            .map(body -> body.protocol().carrier())
            .filter(forma -> !forma.isEmpty())
            .distinct()
            .collect(Collectors.toList());
        if (formas.isEmpty()) {
            throw new IllegalStateException(
                "The program never answers, since every body of it resumes another"
            );
        }
        if (formas.size() > 1) {
            throw new IllegalStateException(
                String.format(
                    "The bodies of the program answer different formas: %s",
                    String.join(", ", formas)
                )
            );
        }
        return formas.get(0);
    }

    /**
     * Whether the Java of the program needs a loop.
     * @return True if a body resumes any body
     */
    public boolean repeats() {
        return this.parts.size() > 1 || this.parts.get(0).protocol().repeats();
    }
}
