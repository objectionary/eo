/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import java.util.List;

/**
 * What a fragment computes, as a program of steps.
 *
 * <p>A reduction settles into this: the steps in their dependency order,
 * the key of the value the fragment answers with — the last step,
 * usually, though a fragment may also collapse into a literal or answer
 * one of its voids unchanged — and the forma of that value. A step is an
 * application or a {@link Fork}, and a fork holds one protocol of this
 * very kind per arm, so a program with choices in it is a tree of
 * protocols whose every path is straight. A path may also end by
 * repeating instead of answering: a fragment that calls itself, or a
 * recursive helper of the formation, in a tail position settles into
 * the name of the body it resumes and the keys the voids of that body
 * take next, one per void in declaration order, and that body runs
 * over them. This is the whole input of code generation: rendering each step
 * as one Java statement, in order, with a block under each arm and a
 * loop around a program that repeats, is a faithful compilation of the
 * fragment.</p>
 *
 * @since 0.76.0
 */
public final class Protocol {

    /**
     * The steps, in their dependency order.
     */
    private final List<Step> steps;

    /**
     * The key of the value the fragment answers with.
     */
    private final String root;

    /**
     * The forma of that value.
     */
    private final String forma;

    /**
     * The name of the body the fragment resumes, empty for the formation
     * itself, when it repeats instead of answering.
     */
    private final String body;

    /**
     * The keys of the values the voids take next, when the fragment
     * repeats instead of answering.
     */
    private final List<String> next;

    /**
     * Ctor.
     * @param moves The steps, in their dependency order
     * @param answer The key of the value the fragment answers with
     * @param carrier The forma of that value
     */
    public Protocol(final List<Step> moves, final String answer, final String carrier) {
        this(moves, answer, carrier, "", Collections.emptyList());
    }

    /**
     * Ctor, for a program that repeats the formation itself.
     * @param moves The steps, in their dependency order
     * @param again The keys of the values the voids take next, in
     *  declaration order
     */
    public Protocol(final List<Step> moves, final List<String> again) {
        this(moves, "", again);
    }

    /**
     * Ctor, for a program that resumes a body.
     * @param moves The steps, in their dependency order
     * @param target The name of the body resumed, empty for the formation
     * @param again The keys of the values the voids of that body take
     *  next, in declaration order
     */
    public Protocol(final List<Step> moves, final String target, final List<String> again) {
        this(moves, "", "", target, again);
    }

    /**
     * Ctor.
     * @param moves The steps, in their dependency order
     * @param answer The key of the value the fragment answers with
     * @param carrier The forma of that value
     * @param target The name of the body resumed, empty for the formation
     * @param again The keys of the values the voids take next
     */
    private Protocol(final List<Step> moves, final String answer,
        final String carrier, final String target, final List<String> again) {
        this.steps = moves;
        this.root = answer;
        this.forma = carrier;
        this.body = target;
        this.next = again;
    }

    /**
     * The steps.
     * @return The steps, in their dependency order
     */
    public List<Step> moves() {
        return Collections.unmodifiableList(this.steps);
    }

    /**
     * The key of the value the fragment answers with.
     * @return A key such as {@code sym:s2} or {@code number:40-14-...},
     *  empty when the program repeats
     */
    public String answer() {
        return this.root;
    }

    /**
     * The forma of the value.
     * @return One of {@code number}, {@code bool}, {@code bytes}, empty
     *  when the program repeats
     */
    public String carrier() {
        return this.forma;
    }

    /**
     * The name of the body the program resumes.
     * @return The name of the helper, empty for the formation itself or
     *  when the program answers
     */
    public String target() {
        return this.body;
    }

    /**
     * The keys of the values the voids of the resumed body take next.
     * @return One key per void of that body, in declaration order, or
     *  none when the program answers
     */
    public List<String> again() {
        return Collections.unmodifiableList(this.next);
    }

    /**
     * Whether this program, or an arm nested anywhere in it, ends by
     * repeating.
     * @return True if the Java of it needs a loop
     */
    public boolean repeats() {
        return !this.next.isEmpty()
            || this.steps.stream()
                .flatMap(step -> step.branches().stream())
                .anyMatch(Protocol::repeats);
    }
}
