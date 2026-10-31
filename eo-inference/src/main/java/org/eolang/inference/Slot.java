/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * A void of a copy, the key of its row.
 *
 * @param copy The copy
 * @param name The void
 * @since 0.76.0
 */
record Slot(int copy, String name) {
}
