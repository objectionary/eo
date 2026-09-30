/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.nio.file.Path;

/**
 * The full name of an entry, turned into the path of its protocol file.
 *
 * <p>Every object in EO has a full name that starts with {@code Φ}, such as
 * {@code Φ.bytes.as-hex}. This full name is called a "locator". When phino
 * works on an entry, it writes down every step it takes into a file, which
 * is called the "protocol" of the entry. This class decides where that file
 * is. It drops the {@code Φ} at the start, turns every dot into a slash,
 * and adds {@code .xml} at the end. So, the protocol of
 * {@code Φ.bytes.as-hex} is the file {@code bytes/as-hex.xml}.</p>
 *
 * <p>Two stages use this class: {@link Morphing}, which writes the
 * protocols, and {@link Rendering}, which reads them. Because both use
 * the same class, they always agree on the name of the file.</p>
 *
 * @since 0.74.0
 */
final class Locator {

    /**
     * The locator, written the same way as in the file {@code entries.tsv}.
     */
    private final String text;

    /**
     * Ctor.
     *
     * @param loc The locator, written the same way as in {@code entries.tsv}
     */
    Locator(final String loc) {
        this.text = loc;
    }

    /**
     * The path of the protocol file, inside the directory of all protocols.
     *
     * @return The path of the protocol file
     */
    Path protocol() {
        if (!this.text.startsWith("Φ.")) {
            throw new IllegalStateException(
                String.format(
                    "The locator '%s' does not start with 'Φ.', while its protocol is named after the path below it",
                    this.text
                )
            );
        }
        return Path.of(String.format("%s.xml", this.text.substring(2).replace('.', '/')));
    }
}
