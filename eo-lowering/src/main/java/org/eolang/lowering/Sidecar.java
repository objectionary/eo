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
 * The sidecar file of one lowered fragment.
 *
 * <p>The Java body of a lowered fragment lands in a file named by the
 * digest of that body, so fragments with the same Java share one file and
 * the name vouches for the content. The passes run in parallel across the
 * XMIRs of one build, which means two of them can produce the same digest
 * at once: writing straight into the final path would let one of them, or
 * any concurrent reader, observe a truncated file. The body therefore
 * goes into a sibling temporary file first and is renamed into place
 * atomically, so the digest path only ever holds a complete body, and a
 * path that already exists is left alone. On Windows the rename can be
 * refused while a rival holds a handle on the target; the rival is
 * writing the same bytes, so the refusal is forgiven once the target
 * exists.</p>
 *
 * @since 0.76.0
 */
public final class Sidecar {

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
     * @param dir The directory for the sidecar bodies
     * @param text The body to save
     */
    public Sidecar(final Path dir, final String text) {
        this.home = dir;
        this.body = text;
    }

    /**
     * Save the body under its digest name.
     * @return The digest the file is named by
     * @throws IOException If saving fails
     */
    public String save() throws IOException {
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
