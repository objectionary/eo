/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * Where code stands: a place, under the tag of the copy around it (design
 * doc 3.6).
 *
 * <p>A read, a call and a stamp happen at a site, and a copy is born at
 * one.</p>
 *
 * @param place The place
 * @param tag The tag, empty when none
 * @since 0.76.0
 */
record Site(String place, String tag) {
}
