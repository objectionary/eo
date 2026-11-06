/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * One argument of a call: the place it stands at, and the void it is bound
 * to, by the number of its slot or by the name of the void.
 *
 * @param loc The place of the argument
 * @param slot The number of its slot, below zero when it names its void
 * @param name The void it names, empty when it goes by its slot
 * @since 0.76.0
 */
record Argument(String loc, int slot, String name) {
}
