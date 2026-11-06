/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * One attribute of a formation, as {@code facts.xml} states it.
 *
 * <p>An attribute is a void or not, and a void may declare the formation
 * it holds ({@code ? > code /Q.number}), which it then answers in every
 * copy, next to what calls put into it (design doc 4.3, declared
 * types).</p>
 *
 * @param open Whether it is a void
 * @param holds The formation it is declared to hold, empty when none
 * @since 0.76.0
 */
record Attribute(boolean open, String holds) {
}
