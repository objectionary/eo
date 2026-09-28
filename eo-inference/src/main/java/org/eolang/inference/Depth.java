/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * How much of a program the tables turned out to say.
 *
 * <p>This reads the tables back and puts every object of the program on the
 * ladder {@link Answers} describes. It is a measurement of ourselves rather than a
 * fact about the program, so what comes out is no table: the goal says it in
 * the log and leaves {@link Ladder#lines()} in a file beside the tables
 * rather than among them, where two builds of the same sources can be compared
 * by anybody.</p>
 *
 * @since 0.69.0
 */
public final class Depth {

    /**
     * The directory with the prepared XMIR files of the program.
     */
    private final Path world;

    /**
     * The directory with the tables.
     */
    private final Path tables;

    /**
     * Ctor.
     *
     * @param xmirs The directory with the prepared XMIR files
     * @param rows The directory with the tables
     */
    public Depth(final Path xmirs, final Path rows) {
        this.world = xmirs;
        this.tables = rows;
    }

    /**
     * How much of the program was understood.
     *
     * @return The objects of the program, counted by the rung they stand on,
     *  and the ones rooted at a void counted by their band as well
     * @throws IOException If a table or a file cannot be read
     */
    public Ladder ladder() throws IOException {
        final List<String> names = Arrays.asList(
            "nothing at all",
            "a name rooted at a void",
            "a formation, voids still free",
            "a formation, nothing left free",
            "nothing left to find out"
        );
        final Map<String, String> shades = new LinkedHashMap<>(0);
        shades.put("unfilled", "rooted at a void nobody fills");
        shades.put("rooted", "rooted at a void the callers fill");
        shades.put("atom", "rooted at a void only an atom fills");
        final Map<String, Integer> counts = new LinkedHashMap<>(names.size());
        for (final String name : names) {
            counts.put(name, 0);
        }
        final Map<String, Integer> bands = new LinkedHashMap<>(shades.size());
        for (final String shade : shades.values()) {
            bands.put(shade, 0);
        }
        int picked = 0;
        for (final Answer answer : new Answered(this.world, this.tables).all().values()) {
            final String name = names.get(answer.rung());
            counts.put(name, counts.get(name) + 1);
            if (!answer.arms().isEmpty()) {
                picked = picked + 1;
            }
            final Band band = new Band(answer);
            if (band.hollow()) {
                final String shade = shades.get(band.name());
                bands.put(shade, bands.get(shade) + 1);
            }
        }
        return new Ladder(counts, picked, bands);
    }
}
