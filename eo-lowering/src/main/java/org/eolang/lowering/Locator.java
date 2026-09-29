/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.nio.file.Path;

/**
 * The locator of an entry, as the path of the file its protocol is in.
 *
 * <p>A protocol is kept at the path the locator of its formation names
 * below {@code Φ}, so that the protocol of {@code Φ.bytes.as-hex} is
 * {@code bytes/as-hex.xml}, and the morphing that writes it and the
 * rendering that reads it find the same file.</p>
 *
 * @since 0.74.0
 */
final class Locator {

    /**
     * The locator, as {@code entries.tsv} holds it.
     */
    private final String text;

    /**
     * Ctor.
     *
     * @param loc The locator, as {@code entries.tsv} holds it
     */
    Locator(final String loc) {
        this.text = loc;
    }

    /**
     * The path of the protocol, relative to the directory of protocols.
     *
     * @return The path
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
