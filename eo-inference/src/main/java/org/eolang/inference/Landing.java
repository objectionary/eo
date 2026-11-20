/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * Where one argument of a call goes for one head: the void it fills, or no
 * void.
 *
 * @param arg The position of the argument
 * @param name The void, empty for no void
 * @since 0.76.0
 */
record Landing(int arg, String name) {
}
