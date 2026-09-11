/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

/**
 * The fires the engine serves, chosen by the λ name phino reports.
 *
 * <p>The registry hands the engine four kinds of λ: the primitives of
 * {@code ops.tsv}, {@code L_dataized}, {@code L_fork} and the boxes
 * planted on the formations of the universe. Anything else is a name the
 * registry should not have matched, and the fire fails loudly.</p>
 *
 * @since 0.76.0
 */
public final class Fires {

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
    public Fires(final Symbols symbols, final Boxes planted, final Channel wire) {
        this.table = symbols;
        this.boxes = planted;
        this.channel = wire;
    }

    /**
     * The fire of one request.
     *
     * @param id The id of the fire
     * @param lambda The λ name
     * @param body The bindings of the formation, as phino spelled them
     * @return The fire
     */
    public Fire at(final int id, final String lambda, final String body) {
        final Operands args = new Operands(id, new Bindings(body), this.channel, this.table);
        final Fire out;
        if ("L_dataized".equals(lambda)) {
            out = new Dataizing(args);
        } else if ("L_fork".equals(lambda)) {
            out = new Forking(args, this.table);
        } else if (lambda.startsWith("L_box_")) {
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
