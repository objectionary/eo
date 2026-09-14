/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import java.util.List;

/**
 * What a fragment computes, as a straight program of steps.
 *
 * <p>It takes the steps in dependency order and how the program ends: with
 * the key of an answer and its forma, by repeating a body with new values,
 * or by failing with a reason. It answers all of that. Every step reads
 * only keys minted before it, and a {@link Fork} holds one protocol of
 * this kind per arm, so a program with choices is a tree whose every path
 * is straight. This is the whole input of code generation.</p>
 *
 * @since 0.76.0
 */
final class Protocol {

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
     * The key of the reason the fragment fails with, when it fails
     * instead of answering.
     */
    private final String cause;

    /**
     * Ctor.
     *
     * @param moves The steps, in their dependency order
     * @param answer The key of the value the fragment answers with
     * @param carrier The forma of that value
     */
    Protocol(final List<Step> moves, final String answer, final String carrier) {
        this(moves, answer, carrier, "", Collections.emptyList(), "");
    }

    /**
     * Ctor, for a program that fails.
     *
     * @param moves The steps, in their dependency order
     * @param reason The key of the reason the fragment fails with
     */
    Protocol(final List<Step> moves, final String reason) {
        this(moves, "", "", "", Collections.emptyList(), reason);
    }

    /**
     * Ctor, for a program that repeats the formation itself.
     *
     * @param moves The steps, in their dependency order
     * @param again The keys of the values the voids take next, in
     *  declaration order
     */
    Protocol(final List<Step> moves, final List<String> again) {
        this(moves, "", again);
    }

    /**
     * Ctor, for a program that resumes a body.
     *
     * @param moves The steps, in their dependency order
     * @param target The name of the body resumed, empty for the formation
     * @param again The keys of the values the voids of that body take
     *  next, in declaration order
     */
    Protocol(final List<Step> moves, final String target, final List<String> again) {
        this(moves, "", "", target, again, "");
    }

    /**
     * Ctor.
     *
     * @param moves The steps, in their dependency order
     * @param answer The key of the value the fragment answers with
     * @param carrier The forma of that value
     * @param target The name of the body resumed, empty for the formation
     * @param again The keys of the values the voids take next
     * @param reason The key of the reason the fragment fails with
     */
    private Protocol(final List<Step> moves, final String answer,
        final String carrier, final String target, final List<String> again,
        final String reason) {
        this.steps = moves;
        this.root = answer;
        this.forma = carrier;
        this.body = target;
        this.next = again;
        this.cause = reason;
    }

    /**
     * The steps.
     *
     * @return The steps, in their dependency order
     */
    List<Step> moves() {
        return Collections.unmodifiableList(this.steps);
    }

    /**
     * The key of the value the fragment answers with.
     *
     * @return A key such as {@code sym:s2} or {@code number:40-14-...},
     *  empty when the program repeats or fails
     */
    String answer() {
        return this.root;
    }

    /**
     * The forma of the value.
     *
     * @return One of {@code number}, {@code bool}, {@code bytes}, empty
     *  when the program repeats or fails
     */
    String carrier() {
        return this.forma;
    }

    /**
     * The name of the body the program resumes.
     *
     * @return The name of the helper, empty for the formation itself or
     *  when the program answers
     */
    String target() {
        return this.body;
    }

    /**
     * The keys of the values the voids of the resumed body take next.
     *
     * @return One key per void of that body, in declaration order, or
     *  none when the program answers
     */
    List<String> again() {
        return Collections.unmodifiableList(this.next);
    }

    /**
     * The key of the reason the program fails with.
     *
     * @return A key such as {@code sym:s3} or {@code string:68-69-},
     *  empty when the program answers or repeats
     */
    String reason() {
        return this.cause;
    }

    /**
     * Whether this program, or an arm nested anywhere in it, ends by
     * repeating.
     *
     * @return True if the Java of it needs a loop
     */
    boolean repeats() {
        return !this.next.isEmpty()
            || this.steps.stream()
                .flatMap(step -> step.branches().stream())
                .anyMatch(Protocol::repeats);
    }
}
