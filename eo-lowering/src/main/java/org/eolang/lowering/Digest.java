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
 * The name of a sidecar file, taken from what is inside it.
 *
 * <p>It takes the Java body of a lowered fragment and answers twelve hex
 * characters of its SHA-256, the shape {@code lowered.xsl} expects in the
 * {@code lowered} attribute. Two fragments with the same Java get the same
 * name, so they share one file.</p>
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
     *
     * @param body The content to digest
     */
    public Digest(final String body) {
        this.text = body;
    }

    /**
     * The digest.
     *
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
