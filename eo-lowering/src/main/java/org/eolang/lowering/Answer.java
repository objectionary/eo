/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import javax.json.JsonObject;

/**
 * One answer of phino to a question, as the facts it carries.
 *
 * <p>phino answers a by-reference question with the node under 𝑛 and,
 * next to it, what the node carries: its bytes under Δ, the λ it is stuck
 * on, whether the attribute is void and, for a node as written, the
 * global object it is dispatched by under Φ. The engine reads those
 * facts and never the node, which is kept to name what went wrong.</p>
 *
 * @since 0.77.0
 */
final class Answer {

    /**
     * The facts.
     */
    private final JsonObject facts;

    /**
     * Ctor.
     *
     * @param message The line phino answered with
     */
    Answer(final JsonObject message) {
        this.facts = message;
    }

    /**
     * The bytes the node carries.
     *
     * @return The bytes, such as {@code 40-08-00-00-00-00-00-00}, or an
     *  empty string when the node is no datum
     */
    String data() {
        return this.facts.getString("Δ", "");
    }

    /**
     * The λ the node is stuck on.
     *
     * @return The name, such as {@code S4}, or an empty string when the
     *  node carries no λ
     */
    String lambda() {
        return this.facts.getString("λ", "");
    }

    /**
     * Whether the attribute asked about is void.
     *
     * @return True if the receiver binds the attribute to nothing at all
     */
    boolean vacant() {
        return this.facts.getBoolean("∅", false);
    }

    /**
     * The global object the node is dispatched by, as written.
     *
     * @return The chain after Φ, such as {@code number} for a number
     *  literal, or an empty string when the node is no such dispatch
     */
    String head() {
        return this.facts.getString("Φ.", "");
    }

    /**
     * The node, as phino spelled it.
     *
     * @return The φ-expression, for a message about it
     */
    String node() {
        return this.facts.getString("𝑛", "");
    }
}
