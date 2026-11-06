/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import org.cactoos.Scalar;

/**
 * The tables of {@code eo:inference} that one YAML pack of the tests reads.
 *
 * <p>Most packs get what the engine itself writes for their EO sources,
 * the key {@code eo}, as {@link Inferred} runs it. A pack that writes the
 * key {@code facts} or {@code answers} gets the tables as it wrote them,
 * with an empty document for the one it leaves out: that is for a case the
 * engine cannot be made to reach with a program, and the pack says why.</p>
 *
 * @since 0.64.0
 */
final class Tables implements Scalar<Path> {

    /**
     * The directory to work in.
     */
    private final Path home;

    /**
     * The content of the YAML pack.
     */
    private final Map<String, Object> pack;

    /**
     * Ctor.
     *
     * @param dir The directory to work in
     * @param yaml The content of the YAML pack
     */
    Tables(final Path dir, final Map<String, Object> yaml) {
        this.home = dir;
        this.pack = yaml;
    }

    @Override
    public Path value() throws IOException {
        final Path made;
        if (this.pack.containsKey("facts") || this.pack.containsKey("answers")) {
            made = Files.createDirectories(this.home.resolve("tables"));
            Files.write(
                made.resolve("facts.xml"),
                this.pack.getOrDefault("facts", "<facts/>").toString()
                    .getBytes(StandardCharsets.UTF_8)
            );
            Files.write(
                made.resolve("answers.xml"),
                this.pack.getOrDefault("answers", "<answers/>").toString()
                    .getBytes(StandardCharsets.UTF_8)
            );
        } else {
            final Map<String, String> sources = new LinkedHashMap<>(0);
            for (final Map.Entry<?, ?> source
                : ((Map<?, ?>) this.pack.getOrDefault("eo", new HashMap<>(0))).entrySet()) {
                sources.put(source.getKey().toString(), source.getValue().toString());
            }
            made = new Inferred(this.home.resolve("engine"), sources).value();
        }
        return made;
    }
}
