/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The operands of one fire.
 *
 * <p>An operand is first read straight off the {@code 𝑏} phino handed
 * over, which works whenever the deep walk has already reduced it to a
 * marker or a datum and costs a regex. When that fails the fire asks, and
 * blocks until phino answers with the reduced node itself. A datum that
 * came back untyped takes the forma the operation expects of it, since a
 * Δ formation carries no type of its own.</p>
 *
 * @since 0.76.0
 */
public final class Operands {

    /**
     * A tuple whose length is a marker.
     */
    private static final Pattern LENGTH = Pattern.compile(
        "length ↦ Φ\\.number\\( φ ↦ Φ\\.bytes\\( φ ↦ ⟦ λ ⤍ (S\\d+) ⟧ \\) \\)"
    );

    /**
     * The id of the fire.
     */
    private final int fire;

    /**
     * The bindings of the fire.
     */
    private final Bindings body;

    /**
     * The wire.
     */
    private final Channel channel;

    /**
     * The table.
     */
    private final Symbols table;

    /**
     * Ctor.
     *
     * @param id The id of the fire
     * @param bindings The bindings of the fire
     * @param wire The wire
     * @param symbols The table
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    public Operands(final int id, final Bindings bindings, final Channel wire,
        final Symbols symbols) {
        this.fire = id;
        this.body = bindings;
        this.channel = wire;
        this.table = symbols;
    }

    /**
     * Whether the name is bound at all.
     *
     * @param name The name of the attribute
     * @return True if the fire binds it to something other than a void
     */
    public boolean bound(final String name) {
        final String value = this.body.of(name);
        return !value.isEmpty() && !"∅".equals(value);
    }

    /**
     * The key of a reduced operand.
     *
     * @param name The name of the attribute
     * @param forma The forma the operation expects, or an empty string
     * @return The key, {@code sym:S4} or {@code number:HEX}
     * @throws IOException If the wire fails
     * @throws InterruptedException If the wait is interrupted
     */
    public String of(final String name, final String forma)
        throws IOException, InterruptedException {
        String key = new Operand(this.body.of(name)).key();
        if (key.isEmpty()) {
            final String reply = this.channel.ask(this.fire, name, true);
            key = new Operand(reply).key();
            if (key.isEmpty()) {
                throw new IllegalStateException(
                    String.format(
                        "The operand '%s' of fire #%d is neither a symbol nor data: %s",
                        name, this.fire, reply
                    )
                );
            }
        }
        if (key.startsWith("bytes:") && !forma.isEmpty() && !"object".equals(forma)) {
            key = String.format("%s:%s", forma, key.substring(6));
        }
        return key;
    }

    /**
     * The key of the receiver, asked for as written.
     *
     * @param lambda The λ name of the box asking, which its lexical parent holds
     * @return The key, or an empty string when the receiver is the lexical parent
     * @throws IOException If the wire fails
     * @throws InterruptedException If the wait is interrupted
     */
    public String receiver(final String lambda) throws IOException, InterruptedException {
        final String reply = this.channel.ask(this.fire, "ρ", false)
            .replaceAll("\\s+", " ");
        String key = new Operand(reply).key();
        if (key.isEmpty()) {
            final Matcher length = Operands.LENGTH.matcher(reply);
            if (length.find()) {
                key = this.table.row(length.group(1)).get(3);
            }
        }
        if (key.isEmpty() && !reply.contains(String.format("λ ⤍ %s", lambda))) {
            throw new IllegalStateException(
                String.format(
                    "The receiver of fire #%d is neither a value nor the parent of '%s': %s",
                    this.fire, lambda, reply
                )
            );
        }
        return key;
    }
}
