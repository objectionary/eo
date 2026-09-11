/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The layout of the directory one build lowers in: the table of boxes,
 * the boxed variant of every document, the sidecar bodies of the atoms,
 * and a scratch directory per run.
 *
 * @since 0.77.0
 */
public final class Home {

    /**
     * The root.
     */
    private final Path dir;

    /**
     * Ctor.
     *
     * @param root The root
     */
    public Home(final Path root) {
        this.dir = root;
    }

    /**
     * The table of boxes.
     *
     * @return The file
     */
    public Path boxes() {
        return this.dir.resolve("boxes.tsv");
    }

    /**
     * The boxed variant of a document, with its tests trimmed.
     *
     * @param name The identifier of the document
     * @return The file
     */
    public Path boxed(final String name) {
        return this.dir.resolve("boxed").resolve(String.format("%s.xmir", name));
    }

    /**
     * The boxed variants of every document but one.
     *
     * @param name The identifier of the document left out
     * @return The files, in the order of their names
     * @throws IOException If the directory cannot be listed
     */
    public List<Path> others(final String name) throws IOException {
        final Path own = this.boxed(name);
        try (Stream<Path> files = Files.list(own.getParent())) {
            return files
                .filter(file -> file.toString().endsWith(".xmir") && !file.equals(own))
                .sorted()
                .collect(Collectors.toList());
        }
    }

    /**
     * The directory with the sidecar bodies.
     *
     * @return The directory
     */
    public Path atoms() {
        return this.dir.resolve("atoms");
    }

    /**
     * A fresh scratch directory for one run.
     *
     * @return The directory
     * @throws IOException If it cannot be made
     */
    public Path run() throws IOException {
        return Files.createTempDirectory(
            Files.createDirectories(this.dir.resolve("runs")), "run"
        );
    }
}
