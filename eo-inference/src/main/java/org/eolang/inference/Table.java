/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;

/**
 * One of the tables where the checker keeps what it knows about types.
 *
 * <p>The checker never renames a type and never merges two of them:
 * every object keeps the type it was born with, and everything we learn
 * goes into a table instead. Three of them are planned — what an object
 * certainly has ({@link Provides}), what it must have judging by how it
 * is used, and which types are copies of which — and every one of them
 * is a document of its own. That is the whole point of the design: a
 * smarter rule adds rows to a table, or reads them differently, and no
 * other part of the pipeline has to change.</p>
 *
 * @since 0.67.0
 */
@FunctionalInterface
interface Table {

    /**
     * The rows of this table.
     * @return The table as an XML document
     */
    XML asXml();
}
