/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.AccessDeniedException;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

/**
 * The file holding the Java body of one lowered fragment.
 *
 * <p>It takes the directory of the bodies and the body itself. It writes
 * the file, named after the digest of the content, and answers that name.
 * Two fragments with the same Java share one file. The write goes through
 * a temporary neighbour and an atomic rename, so a reader never sees half
 * a body while passes run in parallel.</p>
 *
 * @since 0.76.0
 */
final class Sidecar {

    /**
     * The directory for the sidecar bodies.
     */
    private final Path home;

    /**
     * The body to save.
     */
    private final String body;

    /**
     * Ctor.
     *
     * @param dir The directory for the sidecar bodies
     * @param text The body to save
     */
    Sidecar(final Path dir, final String text) {
        this.home = dir;
        this.body = text;
    }

    /**
     * Save the body under its digest name.
     *
     * @return The digest the file is named by
     * @throws IOException If saving fails
     */
    String save() throws IOException {
        final String digest = new Digest(this.body).hex();
        final Path target = this.home.resolve(String.format("%s.java", digest));
        if (!Files.exists(target)) {
            Files.createDirectories(this.home);
            final Path temp = Files.createTempFile(this.home, digest, ".tmp");
            try {
                Files.write(temp, this.body.getBytes(StandardCharsets.UTF_8));
                Sidecar.moved(temp, target);
            } finally {
                Files.deleteIfExists(temp);
            }
        }
        return digest;
    }

    private static void moved(final Path temp, final Path target) throws IOException {
        try {
            Files.move(
                temp, target,
                StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING
            );
        } catch (final AtomicMoveNotSupportedException ex) {
            Files.move(temp, target, StandardCopyOption.REPLACE_EXISTING);
        } catch (final AccessDeniedException ex) {
            if (!Files.exists(target)) {
                throw ex;
            }
        }
    }
}
