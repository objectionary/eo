/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * Something that waits for a stop to decide it (design doc 4.3).
 *
 * <ul>
 *   <li>{@code b}: a call at a place in a copy, on a head, whose argument
 *   is only an open void;</li>
 *   <li>{@code e}: an object, in {@code value}, whose {@code @} gives
 *   nothing typed;</li>
 *   <li>{@code 3}: an argument, by its position in {@code value}, of a call
 *   at a place in a copy, whose every value fails.</li>
 * </ul>
 *
 * @param rule The rule
 * @param place The place of the call, empty for rule e
 * @param ctx The copy around, -1 for rule e
 * @param value The head, the object, or the position of the argument
 * @since 0.76.0
 */
record Wish(char rule, String place, int ctx, int value) {
}
