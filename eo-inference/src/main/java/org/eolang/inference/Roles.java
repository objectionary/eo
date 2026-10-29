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
 * <li>A <b>decorator</b> (4.2) has only voids, a {@code @} and hidden
 * {@code >>} helpers of its own, and reduces to what its {@code @}
 * gives.</li>
 * <li>An <b>atom</b> has no {@code @} and says what Java returns: a
 * formation, or its own voids it names, which it gives back.</li>
 * <li>A <b>test</b> is a formation named {@code p🌵…} or {@code n🌵…}.</li>
 * <li>An object whose {@code @} is one of its own voids (3.5, call then
 * narrow) passes that void on, and its call answers what goes in
 * there.</li>
 * <li>A formation an atom <b>returns</b> is made by Java, and its voids
 * with no declared type are read from the pool (4.5).</li>
 * <li>A read is <b>inner</b> when it reads through {@code ξ}, {@code ^}
 * or {@code ^.^} into the copy's own formations, which is the copy's own
 * code and does not make it count (3.3).</li>
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
     * Whether each formation is a test, worked out once.
     */
    private final Map<String, Boolean> tests;

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
        this.tests = new HashMap<>(0);
    }

    /**
     * Whether a formation is a decorator.
     *
     * @param form The formation
     * @return TRUE if it is
     */
    boolean decorator(final String form) {
        return this.decor.computeIfAbsent(form, this::decorates);
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
        return this.owns.computeIfAbsent(form, this::owned);
    }

    /**
     * Whether a formation is a test.
     *
     * @param form The formation
     * @return TRUE if it is
     */
    boolean test(final String form) {
        return this.tests.computeIfAbsent(
            form, key -> Roles.hidden(key.substring(key.lastIndexOf('.') + 1), false)
        );
    }

    /**
     * The void an object passes on, if its {@code @} is one of its own
     * voids (3.5, call then narrow).
     *
     * @param form The formation
     * @return The void, empty when it passes none on
     */
    String passes(final String form) {
        return this.pass.computeIfAbsent(form, this::passed);
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
        return this.voids.computeIfAbsent(true, key -> this.collected());
    }

    /**
     * Whether a read is inner: it reads through {@code ξ}, {@code ^} or
     * {@code ^.^} into a formation of the copy's own (3.3).
     *
     * @param place The place of the read
     * @return TRUE if it is
     */
    boolean inner(final String place) {
        return this.inners.computeIfAbsent(place, this::through);
    }

    private boolean decorates(final String form) {
        final Formation shape = this.program.form(form);
        return shape.has("φ") && !shape.open("φ")
            && shape.names().stream().allMatch(name -> Roles.fits(shape, name));
    }

    private List<String> owned(final String form) {
        final List<String> known = new ArrayList<>(0);
        if (this.atom(form)) {
            final Formation shape = this.program.form(form);
            List<String> locs = shape.gives();
            if (locs.isEmpty()) {
                locs = Collections.singletonList(shape.returns());
            }
            final String prefix = form.concat(".");
            for (final String loc : locs) {
                if (loc.startsWith(prefix) && shape.has(loc.substring(prefix.length()))) {
                    known.add(loc.substring(prefix.length()));
                }
            }
        }
        return known;
    }

    private String passed(final String form) {
        final String phi = String.join(".", form, "φ");
        final String found;
        if (this.program.form(form).has("φ") && !this.program.calls(phi)
            && this.program.reads(phi)) {
            found = this.narrowed(form, this.program.read(phi));
        } else {
            found = "";
        }
        return found;
    }

    private String narrowed(final String form, final Read read) {
        final Formation shape = this.program.form(form);
        final String found;
        if (this.self(form, read.of()) && shape.order().contains(read.name())
            && Roles.plain(shape)) {
            found = read.name();
        } else {
            found = "";
        }
        return found;
    }

    private boolean self(final String form, final String place) {
        final List<Head> heads = this.program.heads(place);
        return this.program.starts(place) && heads.size() == 1
            && "ref".equals(heads.get(0).tag()) && form.equals(heads.get(0).loc());
    }

    private List<List<String>> collected() {
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

    private boolean through(final String place) {
        String from = this.program.read(place).of();
        while (!this.program.calls(from) && !this.program.starts(from) && this.upward(from)) {
            from = this.program.read(from).of();
        }
        return !this.program.calls(from) && this.program.starts(from) && this.owning(from);
    }

    private boolean upward(final String place) {
        return this.program.reads(place) && "ρ".equals(this.program.read(place).name());
    }

    private boolean owning(final String place) {
        final List<Head> heads = this.program.heads(place);
        return !heads.isEmpty() && heads.stream().allMatch(
            head -> "ref".equals(head.tag()) && !"Φ".equals(head.loc())
                && this.program.formation(head.loc())
        );
    }

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

    private static boolean fits(final Formation shape, final String name) {
        return shape.open(name) || "φ".equals(name) || Roles.hidden(name, true);
    }

    private static boolean hidden(final String name, final boolean more) {
        return name.startsWith("p🌵") || name.startsWith("n🌵")
            || more && name.startsWith("a🌵");
    }
}
