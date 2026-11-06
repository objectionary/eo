/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * What a chain starts from: a formation around, {@code Φ}, a datum or a
 * termination.
 *
 * @param tag What it is: "ref", "data" or "terminator"
 * @param loc The formation or {@code Φ} a "ref" names, empty for the others
 * @since 0.76.0
 */
record Head(String tag, String loc) {
}
