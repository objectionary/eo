/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.atoms;

import org.eolang.lowering.Boxes;
import org.eolang.lowering.Op;
import org.eolang.lowering.Symbols;

/**
 * The fires the engine can serve.
 *
 * <p>It takes the symbol table, the boxes of the build and the wire. Given
 * the id of a fire and the λ name phino reports, it answers the fire that
 * serves it: a primitive of {@code ops.tsv}, the dataization, the fork, or
 * a box. Any other name is one the registry should never have matched, and
 * the fire fails loudly.</p>
 *
 * @since 0.76.0
 */
final class Fires {

    /**
     * The table.
     */
    private final Symbols table;

    /**
     * The boxes.
     */
    private final Boxes boxes;

    /**
     * The wire.
     */
    private final Channel channel;

    /**
     * Ctor.
     *
     * @param symbols The table
     * @param planted The boxes
     * @param wire The wire
     */
    Fires(final Symbols symbols, final Boxes planted, final Channel wire) {
        this.table = symbols;
        this.boxes = planted;
        this.channel = wire;
    }

    /**
     * The fire of one request.
     *
     * @param id The id of the fire
     * @param lambda The λ name
     * @return The fire
     */
    Fire at(final int id, final String lambda) {
        final Operands args = new Operands(id, this.channel, this.table);
        final Fire out;
        if ("L_dataized".equals(lambda)) {
            out = new Dataizing(args);
        } else if ("L_fork".equals(lambda)) {
            out = new Forking(args, this.table);
        } else if (lambda.startsWith("L_box")) {
            out = new Boxing(this.boxes.at(lambda), args, this.table);
        } else if (new Op(lambda).listed()) {
            out = new Primitive(new Op(lambda), args, this.table);
        } else {
            throw new IllegalArgumentException(
                String.format("The λ function '%s' is not one the engine serves", lambda)
            );
        }
        return out;
    }
}
