/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * One thing EO code puts into a void, as {@link Fillings} reads it off
 * {@code facts.xml}.
 *
 * <p>The kinds, by the letter that names them:</p>
 *
 * <ul>
 *   <li>{@code O}, a fresh copy of the formation in {@code loc}: one written
 *   right there, or one a path reads from {@code Φ} down;</li>
 *   <li>{@code C}, a call on a fresh copy of the formation in {@code loc},
 *   a literal when that formation is a kind of the canon;</li>
 *   <li>{@code Φ}, the root; {@code D}, a datum; {@code T}, a
 *   termination;</li>
 *   <li>{@code X}, some copy of the formation in {@code loc} with voids
 *   nobody can name before the walk, such as the copy around;</li>
 *   <li>{@code W}, a thing only the walk can answer, written at
 *   {@code loc}.</li>
 * </ul>
 *
 * @param kind The letter of the kind
 * @param loc The formation, or the place of a thing only the walk answers
 * @since 0.76.0
 */
record Filling(char kind, String loc) {
}
