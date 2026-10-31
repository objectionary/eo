/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * A name read at a place, and the place it is read off.
 *
 * @param name The name read
 * @param of The place it is read off
 * @since 0.76.0
 */
record Read(String name, String of) {
}
