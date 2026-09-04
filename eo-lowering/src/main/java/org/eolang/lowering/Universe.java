/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.IoCheckedText;
import org.cactoos.text.TextOf;

/**
 * The φ-calculus expression holding the method tables of the primitives.
 *
 * <p>Dataizing a fragment needs the tables of the primitive λ-atoms it
 * dispatches into, and phino resolves a {@code Φ.x} reference against the
 * root formation of the document it evaluates. This is that root, read
 * from the {@code universe.phi} resource: {@code number} and {@code bytes}
 * with their twelve λ methods, and {@code true}/{@code false} as data,
 * since the comparing atoms answer with a reference to them. It is a
 * complete expression of its own, merged with an {@link Expression} by
 * {@code phino merge} before dataization; a dispatch into anything it
 * does not hold leaves the dataization stuck, which the caller reads as
 * a refusal to fold.</p>
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
        try {
            return new IoCheckedText(
                new TextOf(
                    new ResourceOf("org/eolang/lowering/universe.phi", this.getClass())
                )
            ).asString();
        } catch (final IOException ex) {
            throw new IllegalStateException(
                "Failed to read universe.phi from classpath", ex
            );
        }
    }
}
