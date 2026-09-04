/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * The name of one sidecar file, computed from its content.
 *
 * <p>A lowered formation and the body of its {@code lambda()} method are
 * written apart — the stamp into the XMIR, the body into a sidecar file —
 * and the digest is what holds them together: twelve hex characters of
 * the SHA-256 of the body, the exact shape {@code lowered.xsl} demands of
 * the {@code lowered} attribute before it reads the file. Naming the file
 * after its content also deduplicates for free: two fragments compiling
 * into the same body share one sidecar, whatever files they came
 * from.</p>
 *
 * @since 0.76.0
 */
public final class Digest {

    /**
     * The content to digest.
     */
    private final String text;

    /**
     * Ctor.
     * @param body The content to digest
     */
    public Digest(final String body) {
        this.text = body;
    }

    /**
     * The digest.
     * @return Twelve hex characters
     */
    public String hex() {
        try {
            return String.format(
                "%064x",
                new BigInteger(
                    1,
                    MessageDigest.getInstance("SHA-256").digest(
                        this.text.getBytes(StandardCharsets.UTF_8)
                    )
                )
            ).substring(0, 12);
        } catch (final NoSuchAlgorithmException ex) {
            throw new IllegalStateException("SHA-256 is not available", ex);
        }
    }
}
