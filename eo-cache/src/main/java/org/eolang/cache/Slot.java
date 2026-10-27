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
 * it was made from, as one file written at once by {@link Saved}, so a reader
 * that finds the sha it expects has already read the content that belongs to
 * it. Kept apart, as a {@code .sha256} beside the content, the two disagree
 * for as long as it takes another build to write both, and the reader gets the
 * content of a source it never asked for (#9174). The cache is machine-wide
 * and this slot is named after the source alone, so the other build is no rare
 * one: two projects holding a {@code foo/app.eo} share this very file.
 *
 * @since 0.75
 */
final class Slot {

    /**
     * What tells the sha from the content: the first space in the file. A sha
     * is Base64 and holds none, while a line separator would be the one of the
     * machine that wrote it, and other machines read this directory too.
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
