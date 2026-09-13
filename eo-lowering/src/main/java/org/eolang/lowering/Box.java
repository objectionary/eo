/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * One box, a row of {@code boxes.tsv}.
 *
 * <p>The row binds the λ name planted on a formation to the locator of
 * that formation, the carrier it is known to answer, the forma of its ρ
 * when the body reaches for it (a dash when it never does), and the names
 * and formas of its voids, {@code x:number y:object}.</p>
 *
 * @since 0.76.0
 */
public final class Box {

    /**
     * The cells of the row.
     */
    private final List<String> cells;

    /**
     * Ctor.
     *
     * @param row The cells of the row
     */
    public Box(final List<String> row) {
        this.cells = row;
    }

    /**
     * The λ name.
     *
     * @return The name, such as {@code L_box_7}
     */
    public String lambda() {
        return this.cells.get(0);
    }

    /**
     * The formation.
     *
     * @return The locator, such as {@code Φ.demo.helper}
     */
    public String locator() {
        return this.cells.get(1);
    }

    /**
     * The carrier the formation answers.
     *
     * @return The carrier, or {@code object} when unknown
     */
    public String carrier() {
        return this.cells.get(2);
    }

    /**
     * Whether the body reaches for its ρ.
     *
     * @return True if the receiver matters to the body
     */
    public boolean reaches() {
        return !"-".equals(this.cells.get(3));
    }

    /**
     * The forma of the ρ.
     *
     * @return The forma, or {@code object} when unknown
     */
    public String parent() {
        return this.cells.get(3);
    }

    /**
     * The voids of the formation.
     *
     * @return The names and their formas, in the order declared
     */
    public Map<String, String> voids() {
        final Map<String, String> out = new LinkedHashMap<>(0);
        if (this.cells.size() > 4) {
            for (final String cell : this.cells.get(4).split(" ")) {
                if (!cell.isEmpty()) {
                    final String[] parts = cell.split(":", 2);
                    out.put(parts[0], parts[1]);
                }
            }
        }
        return out;
    }

    /**
     * The row.
     *
     * @return The tab-separated line
     */
    public String line() {
        return String.join("\t", this.cells);
    }
}
