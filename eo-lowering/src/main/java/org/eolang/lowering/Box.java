/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * One box: a row of the boxes table.
 *
 * <p>It takes the cells of the row, each under its own name, and answers
 * them one by one: the λ name planted on a formation, where that
 * formation is, what it answers, what its receiver carries, and the name
 * and forma of each of its arguments. This is the whole of what one side
 * of the wire knows about a formation the other side may enter.</p>
 *
 * @since 0.76.0
 */
public final class Box {

    /**
     * The cells of the row, by name.
     */
    private final Map<String, String> cells;

    /**
     * Ctor.
     *
     * @param row The cells of the row, by name
     */
    public Box(final Map<String, String> row) {
        this.cells = row;
    }

    /**
     * The λ name.
     *
     * @return The name, such as {@code L_box_7}
     */
    public String lambda() {
        return this.cell("id");
    }

    /**
     * The formation.
     *
     * @return The locator, such as {@code Φ.demo.helper}
     */
    public String locator() {
        return this.cell("locator");
    }

    /**
     * The name the parent holds the formation by.
     *
     * @return The last segment of the locator, such as {@code helper}
     */
    public String name() {
        final String place = this.locator();
        return place.substring(place.lastIndexOf('.') + 1);
    }

    /**
     * The carrier the formation answers.
     *
     * @return The carrier, or {@code object} when unknown
     */
    public String carrier() {
        return this.cell("carrier");
    }

    /**
     * Whether the body reaches for its ρ.
     *
     * @return True if the receiver matters to the body
     */
    public boolean reaches() {
        return !"-".equals(this.parent());
    }

    /**
     * The forma of the ρ.
     *
     * @return The forma, or {@code object} when unknown
     */
    public String parent() {
        return this.cell("parent");
    }

    /**
     * The voids of the formation.
     *
     * @return The names and their formas, in the order declared
     */
    public Map<String, String> voids() {
        final Map<String, String> out = new LinkedHashMap<>(0);
        for (final String cell : this.cell("voids").split(" ", -1)) {
            if (!cell.isEmpty()) {
                final String[] parts = cell.split(":", 2);
                out.put(parts[0], parts[1]);
            }
        }
        return out;
    }

    /**
     * The row.
     *
     * @return The cells, by name
     */
    Map<String, String> row() {
        return this.cells;
    }

    private String cell(final String name) {
        return this.cells.getOrDefault(name, "");
    }
}
