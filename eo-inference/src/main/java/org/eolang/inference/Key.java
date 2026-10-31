/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * A question the memo keeps, or a thing whose growth it watches.
 *
 * <p>The kinds, by the letter that names them:</p>
 *
 * <ul>
 *   <li>{@code a}, a name read off a value at a place under a tag;</li>
 *   <li>{@code e}, the answer at a place in a copy;</li>
 *   <li>{@code k}, what a value reduces to;</li>
 *   <li>{@code u}, a walk of {@code use()}: a value read, dataized or
 *   tested at a place, {@code how} saying which;</li>
 *   <li>{@code w}, the walk of a copy in a pass;</li>
 *   <li>{@code r}, the row of a void of a copy;</li>
 *   <li>{@code p}, the pool of a void of a formation Java makes;</li>
 *   <li>{@code c}, whether a value counts, {@code how} saying what it was
 *   told.</li>
 * </ul>
 *
 * @param kind The letter of the kind
 * @param value The value or copy
 * @param name The name read, or the void
 * @param place The place, or the formation of a pool
 * @param tag The tag, empty when none
 * @param how What a walk does, or what a count was
 * @since 0.76.0
 */
record Key(char kind, int value, String name, String place, String tag, int how) {
}
