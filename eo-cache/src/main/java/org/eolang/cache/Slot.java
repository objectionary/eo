/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.cache;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.Optional;

/**
 * One place in the cache, holding compiled content and the sha of the source
 * it was made from.
 *
 * <p>The two are one file, written at once by {@link Saved}, so a reader that
 * finds the sha it expects has already read the content that belongs to it.
 * Kept apart, as a {@code .sha256} beside the content, they disagree for as
 * long as it takes another build to write both, and a reader that checks the
 * sha and then reads the content gets the content of a source it never asked
 * for (#9174). The cache is machine-wide and the slot of a local source is
 * named after that source alone, so the other build is not a rare one: two
 * projects that both hold a {@code foo/app.eo} share this very file.</p>
 *
 * @since 0.75
 */
final class Slot {

    /**
     * What tells the sha from the content: the first space in the file. A
     * sha is Base64 and holds none of its own, while a line separator would
     * be the one of the machine that wrote the file, and this cache is read
     * by whatever reads that directory.
     */
    private static final String SPLIT = " ";

    /**
     * The file.
     */
    private final Path file;

    /**
     * Ctor.
     *
     * @param path The file to keep the content in
     */
    Slot(final Path path) {
        this.file = path;
    }

    /**
     * The content made from the source with this sha, if that is what is here.
     *
     * @param sha The sha of the source asked about
     * @return The content, or empty when this slot holds something else
     * @throws IOException If fails to read
     */
    Optional<String> of(final String sha) throws IOException {
        Optional<String> result = Optional.empty();
        try {
            final String text = Files.readString(this.file);
            final int split = text.indexOf(Slot.SPLIT);
            if (split > 0 && sha.equals(text.substring(0, split))) {
                result = Optional.of(text.substring(split + 1));
            }
        } catch (final NoSuchFileException ignored) {
            result = Optional.empty();
        }
        return result;
    }

    /**
     * Put the content made from the source with this sha here.
     *
     * @param sha The sha of the source it was made from
     * @param content The content
     * @throws IOException If fails to write
     */
    void put(final String sha, final String content) throws IOException {
        new Saved(String.join(Slot.SPLIT, sha, content), this.file).value();
    }
}
