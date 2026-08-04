/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.tojos.Tojos;

/**
 * One of the tables where the checker keeps what it knows about types.
 *
 * <p>The checker never renames a type and never merges two of them:
 * every object keeps the type it was born with, and everything we learn
 * goes into a table instead. Three of them are planned — what an object
 * certainly has ({@link Provides}), what it must have judging by how it
 * is used, and which types are copies of which — and each one is filled
 * by one rule, out of one kind of object. Keeping them apart like this is
 * what lets the checker grow: a smarter rule adds rows to a table, or
 * reads them differently, and no other part of the pipeline has to
 * change.</p>
 *
 * <p>The rows are {@code Tojos}, the same tables the compiler already
 * keeps its catalogues in. A row is a handful of named cells, which is
 * all a fact about a type ever is, and asking for them back is a
 * {@code select} rather than a hand-written query — the checker's
 * to-do loop will do little else. Thread-safety and caching arrive as
 * decorators when they are needed, and every table can be written out
 * as CSV or JSON for free. The XML the module reports is a view over
 * these rows, built by {@link Grouped}; the rows themselves are the
 * truth.</p>
 *
 * @since 0.67.0
 */
@FunctionalInterface
interface Table {

    /**
     * The rows this table holds.
     * @return The rows
     */
    Tojos rows();
}
