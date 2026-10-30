/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * How a value is used as it is (design doc 3.3): the name read off it, or
 * the void it is handed to, the site of the use, and whether it is read,
 * dataized by an atom or dataized by a test.
 *
 * @param name The name read, or the void it is handed to
 * @param site The site of the use
 * @param how How it is used: {@link Counting#READ}, {@link Counting#DATAIZED}
 *  or {@link Counting#TESTED}
 * @since 0.76.0
 */
record Usage(String name, Site site, int how) {
}
