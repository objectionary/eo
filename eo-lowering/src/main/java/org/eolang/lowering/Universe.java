/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;

/**
 * The φ-calculus expression holding the method tables of the primitives.
 *
 * <p>Dataizing a fragment needs the tables of the primitive λ-atoms it
 * dispatches into, and phino resolves a {@code Φ.x} reference against the
 * root formation of the document it evaluates. This is that root, read
 * from the {@code universe.phi} resource: {@code number} and {@code bytes}
 * with their thirteen λ methods, {@code string} which owns none and reaches
 * every one of them through its {@code φ}, {@code bool} with the one
 * {@code if} that phino never fires but always parks, so that the
 * reduction learns where a choice stands, and {@code true}/{@code false}
 * as data, since the comparing atoms answer with a reference to them. It is
 * a complete expression of its own, merged with an {@link Expression} by
 * {@code phino merge} before dataization; a dispatch into anything it
 * does not hold leaves the dataization stuck, which the caller reads as
 * a refusal to fold.</p>
 *
 * <p>A decorator must shadow whatever its decoratee answers differently,
 * or the wrong atom fires. {@code eo:merge} makes every object of the
 * {@code string} package a real attribute of {@code string}, and one of
 * them — {@code slice}, which counts characters where {@code bytes.slice}
 * counts bytes — names a method this universe models. It therefore stands
 * here bound to a λ no {@link Op} row knows, so a text slicing refuses
 * instead of quietly reaching the bytes atom below it. The same shadowing
 * keeps {@code number.eq} honest: its contract is IEEE 754, where a
 * not-a-number equals nothing and the two zeroes equal each other, while
 * {@code bytes.eq} below it reads eight raw bytes and answers the opposite
 * on both. It stands here bound to a λ of its own, which an {@link Op} row
 * renders as the Java {@code ==} of two doubles, the very comparison the
 * contract asks for.</p>
 *
 * @since 0.76.0
 */
public final class Universe {

    /**
     * Ctor.
     */
    public Universe() {
        // nothing
    }

    /**
     * The expression, in phi syntax.
     * @return The text of the {@code universe.phi} resource
     */
    public String text() {
        return new UncheckedText(
            new TextOf(
                new ResourceOf("org/eolang/lowering/universe.phi", this.getClass())
            )
        ).asString();
    }
}
