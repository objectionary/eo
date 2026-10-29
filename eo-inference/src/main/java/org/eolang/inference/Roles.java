/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * What the facts say of the role each formation plays, before any walk.
 *
 * <ul>
 *   <li>A <b>decorator</b> (4.2) has only voids, a {@code @} and hidden
 *   {@code >>} helpers of its own, and reduces to what its {@code @}
 *   gives.</li>
 *   <li>An <b>atom</b> has no {@code @} and says what Java returns: a
 *   formation, or its own voids it names, which it gives back.</li>
 *   <li>A <b>test</b> is a formation named {@code p🌵…} or {@code n🌵…}.</li>
 *   <li>An object whose {@code @} is one of its own voids (3.5, call then
 *   narrow) passes that void on, and its call answers what goes in
 *   there.</li>
 *   <li>A formation an atom <b>returns</b> is made by Java, and its voids
 *   with no declared type are read from the pool (4.5).</li>
 *   <li>A read is <b>inner</b> when it reads through {@code ξ}, {@code ^}
 *   or {@code ^.^} into the copy's own formations, which is the copy's own
 *   code and does not make it count (3.3).</li>
 * </ul>
 *
 * @since 0.76.0
 */
final class Roles {

    /**
     * The program.
     */
    private final Program program;

    /**
     * Whether each formation is a decorator, worked out once.
     */
    private final Map<String, Boolean> decor;

    /**
     * The void each formation passes on, worked out once.
     */
    private final Map<String, String> pass;

    /**
     * The own voids each atom gives back, worked out once.
     */
    private final Map<String, List<String>> owns;

    /**
     * Whether each read is inner, worked out once.
     */
    private final Map<String, Boolean> inners;

    /**
     * The formations atoms return, worked out once.
     */
    private final Map<Boolean, Set<String>> returned;

    /**
     * The voids of the formations atoms return with no declared type,
     * worked out once.
     */
    private final Map<Boolean, List<List<String>>> voids;

    /**
     * Ctor.
     *
     * @param prog The program
     */
    Roles(final Program prog) {
        this.program = prog;
        this.decor = new HashMap<>(0);
        this.pass = new HashMap<>(0);
        this.owns = new HashMap<>(0);
        this.inners = new HashMap<>(0);
        this.returned = new HashMap<>(1);
        this.voids = new HashMap<>(1);
    }

    /**
     * Whether a formation is a decorator.
     *
     * @param form The formation
     * @return TRUE if it is
     */
    boolean decorator(final String form) {
        Boolean known = this.decor.get(form);
        if (known == null) {
            final Formation shape = this.program.form(form);
            boolean found = shape.has("φ") && !shape.open("φ");
            if (found) {
                for (final String name : shape.names()) {
                    if (!shape.open(name) && !"φ".equals(name) && !Roles.hidden(name, true)) {
                        found = false;
                        break;
                    }
                }
            }
            known = found;
            this.decor.put(form, known);
        }
        return known;
    }

    /**
     * Whether a formation is an atom.
     *
     * @param form The formation
     * @return TRUE if it is
     */
    boolean atom(final String form) {
        final Formation shape = this.program.form(form);
        return !shape.returns().isEmpty() && !shape.has("φ");
    }

    /**
     * The own voids an atom gives back, in the order it names them.
     *
     * @param form The formation
     * @return The voids, none when it is no atom or gives back none of its own
     */
    List<String> own(final String form) {
        List<String> known = this.owns.get(form);
        if (known == null) {
            known = new ArrayList<>(0);
            if (this.atom(form)) {
                final Formation shape = this.program.form(form);
                List<String> locs = shape.gives();
                if (locs.isEmpty()) {
                    locs = Collections.singletonList(shape.returns());
                }
                for (final String loc : locs) {
                    if (loc.startsWith(form + ".")
                        && shape.has(loc.substring(form.length() + 1))) {
                        known.add(loc.substring(form.length() + 1));
                    }
                }
            }
            this.owns.put(form, known);
        }
        return known;
    }

    /**
     * Whether a formation is a test.
     *
     * @param form The formation
     * @return TRUE if it is
     */
    boolean test(final String form) {
        return Roles.hidden(form.substring(form.lastIndexOf('.') + 1), false);
    }

    /**
     * The void an object passes on, if its {@code @} is one of its own
     * voids (3.5, call then narrow).
     *
     * @param form The formation
     * @return The void, empty when it passes none on
     */
    String passes(final String form) {
        String known = this.pass.get(form);
        if (known == null) {
            known = "";
            final Formation shape = this.program.form(form);
            final String at = form + ".φ";
            if (shape.has("φ") && !this.program.calls(at) && this.program.reads(at)) {
                final Read read = this.program.read(at);
                final List<Head> heads = this.program.heads(read.of());
                if (this.program.starts(read.of()) && heads.size() == 1
                    && "ref".equals(heads.get(0).tag()) && form.equals(heads.get(0).loc())
                    && shape.order().contains(read.name()) && Roles.plain(shape)) {
                    known = read.name();
                }
            }
            this.pass.put(form, known);
        }
        return known;
    }

    /**
     * Whether a formation is one an atom returns.
     *
     * @param form The formation
     * @return TRUE if it is
     */
    boolean returned(final String form) {
        return this.returns().contains(form);
    }

    /**
     * The voids of the formations atoms return that have no declared type,
     * as formation and void, in order.
     *
     * @return The pairs
     */
    List<List<String>> javaVoids() {
        return this.voids.computeIfAbsent(
            true,
            key -> {
                final List<List<String>> pairs = new ArrayList<>(0);
                for (final String form : this.returns()) {
                    final Formation shape = this.program.form(form);
                    for (final String name : shape.names()) {
                        if (shape.open(name) && shape.holds(name).isEmpty()) {
                            pairs.add(List.of(form, name));
                        }
                    }
                }
                final CodeOrder order = new CodeOrder();
                pairs.sort(
                    (left, right) -> {
                        int cmp = order.compare(left.get(0), right.get(0));
                        if (cmp == 0) {
                            cmp = order.compare(left.get(1), right.get(1));
                        }
                        return cmp;
                    }
                );
                return pairs;
            }
        );
    }

    /**
     * Whether a read is inner: it reads through {@code ξ}, {@code ^} or
     * {@code ^.^} into a formation of the copy's own (3.3).
     *
     * @param place The place of the read
     * @return TRUE if it is
     */
    boolean inner(final String place) {
        Boolean known = this.inners.get(place);
        if (known == null) {
            String of = this.program.read(place).of();
            boolean found = false;
            while (!this.program.calls(of)) {
                if (this.program.starts(of)) {
                    final List<Head> heads = this.program.heads(of);
                    found = !heads.isEmpty();
                    for (final Head head : heads) {
                        if (!"ref".equals(head.tag()) || "Φ".equals(head.loc())
                            || !this.program.formation(head.loc())) {
                            found = false;
                            break;
                        }
                    }
                    break;
                }
                if (this.program.reads(of) && "ρ".equals(this.program.read(of).name())) {
                    of = this.program.read(of).of();
                    continue;
                }
                break;
            }
            known = found;
            this.inners.put(place, known);
        }
        return known;
    }

    /**
     * The formations atoms return.
     *
     * @return The formations
     */
    private Set<String> returns() {
        return this.returned.computeIfAbsent(
            true,
            key -> {
                final Set<String> found = new LinkedHashSet<>(0);
                for (final String form : this.program.formations()) {
                    final String ret = this.program.form(form).returns();
                    if (this.program.formation(ret)) {
                        found.add(ret);
                    }
                }
                return found;
            }
        );
    }

    /**
     * Whether every attribute of a formation, but its {@code @} and its
     * tests, is a void.
     *
     * @param shape The formation
     * @return TRUE if every one is
     */
    private static boolean plain(final Formation shape) {
        boolean all = true;
        for (final String name : shape.names()) {
            if (!"φ".equals(name) && !Roles.hidden(name, false) && !shape.open(name)) {
                all = false;
                break;
            }
        }
        return all;
    }

    /**
     * Whether a name is one the parser hides: a test, or with {@code more}
     * an object written with {@code >>} too.
     *
     * @param name The name
     * @param more Whether a {@code >>} object counts too
     * @return TRUE if it is
     */
    private static boolean hidden(final String name, final boolean more) {
        return name.startsWith("p🌵") || name.startsWith("n🌵")
            || more && name.startsWith("a🌵");
    }
}
