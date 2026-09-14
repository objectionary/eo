/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.cactoos.Text;
import org.cactoos.text.Split;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;

/**
 * The boxes of one build, a tab-separated file.
 *
 * <p>One box is minted per formation of the universe that declares
 * arguments, and the same file serves every run of the build, so that a
 * formation carries the same λ name in every universe it is written into.
 * The build side writes it once, the engine reads it on every fire of a
 * box.</p>
 *
 * @since 0.76.0
 */
public final class Boxes {

    /**
     * The file.
     */
    private final Path file;

    /**
     * Ctor.
     *
     * @param table The file
     */
    public Boxes(final Path table) {
        this.file = table;
    }

    /**
     * The box of a λ name.
     *
     * @param lambda The name, such as {@code L_box_7}
     * @return The box
     */
    public Box at(final String lambda) {
        return this.all().stream()
            .filter(box -> box.lambda().equals(lambda))
            .findFirst().orElseThrow(
                () -> new IllegalStateException(
                    String.format("No box is planted under the name '%s'", lambda)
                )
            );
    }

    /**
     * The box of a formation.
     *
     * @param locator The locator of the formation
     * @return The λ name, or an empty string when the formation has no box
     */
    public String of(final String locator) {
        return this.all().stream()
            .filter(box -> box.locator().equals(locator))
            .map(Box::lambda)
            .findFirst()
            .orElse("");
    }

    /**
     * All boxes.
     *
     * @return The boxes, in the order planted
     */
    public List<Box> all() {
        final List<Box> out = new ArrayList<>(0);
        if (Files.exists(this.file)) {
            for (final Text line : this.lines()) {
                final String row = new UncheckedText(line).asString();
                if (!row.isEmpty()) {
                    out.add(new Box(Arrays.asList(row.split("\t", -1))));
                }
            }
        }
        return out;
    }

    /**
     * Write all boxes.
     *
     * @param boxes The boxes
     * @throws IOException If the file cannot be written
     */
    public void save(final List<Box> boxes) throws IOException {
        Files.createDirectories(this.file.toAbsolutePath().getParent());
        Files.write(
            this.file,
            boxes.stream().map(Box::line).collect(
                Collectors.joining(System.lineSeparator(), "", System.lineSeparator())
            ).getBytes(StandardCharsets.UTF_8)
        );
    }

    private Iterable<Text> lines() {
        return new Split(new TextOf(this.file), "\\R");
    }
}
