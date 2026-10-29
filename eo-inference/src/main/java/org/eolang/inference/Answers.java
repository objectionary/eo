/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.io.IOException;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

/**
 * The file {@code answers.xml}: what the walk tells of every place
 * (design doc 5.1, {@code eo-inference-answers.md}).
 *
 * <p>One {@code <o>} per place, sorted by its locator code point by code
 * point, with the number of copies that say something there, the sets of
 * labels they say, the most copies first, what the copies of a formation
 * reduce to, and the void an argument lands in. With one set its labels sit
 * right under {@code <o>}. The text is written by hand, one element per line
 * and two spaces per level, so that it is the same bytes the Python engine
 * wrote.</p>
 *
 * @since 0.76.0
 */
public final class Answers {

    /**
     * The walk.
     */
    private final Walk walk;

    /**
     * Ctor.
     *
     * @param source The walk
     */
    public Answers(final Walk source) {
        this.walk = source;
    }

    /**
     * Walk and write the answers into a file.
     *
     * @param file The file
     * @throws IOException If it cannot be written
     */
    public void write(final Path file) throws IOException {
        final Told told = this.walk.told();
        final TreeSet<String> places = new TreeSet<>(new CodeOrder());
        places.addAll(told.sets().keySet());
        places.addAll(told.reduction().keySet());
        places.addAll(told.lands().keySet());
        Files.createDirectories(file.toAbsolutePath().getParent());
        try (Writer out = Files.newBufferedWriter(file, StandardCharsets.UTF_8)) {
            out.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<answers>\n");
            for (final String place : places) {
                final Map<String, Integer> sets = told.sets().getOrDefault(
                    place, Collections.emptyMap()
                );
                final StringBuilder line = new StringBuilder(64)
                    .append("  <o loc=\"").append(Answers.quoted(place))
                    .append("\" copies=\"").append(Answers.sum(sets)).append('"');
                if (told.lands().containsKey(place)) {
                    line.append(" lands=\"").append(Answers.quoted(told.lands().get(place)))
                        .append('"');
                }
                line.append(">\n");
                Answers.groups(sets, "\t", "    ", line);
                if (told.reduction().containsKey(place)) {
                    final Map<String, Integer> reduced = told.reduction().get(place);
                    line.append("    <reduced copies=\"").append(Answers.sum(reduced))
                        .append("\">\n");
                    Answers.groups(reduced, " ", "      ", line);
                    line.append("    </reduced>\n");
                }
                line.append("  </o>\n");
                out.write(line.toString());
            }
            out.write("</answers>\n");
        }
    }

    /**
     * Write the sets of labels, the most copies first, then by their labels
     * compared one by one.
     *
     * @param counted The number of copies of each set, by its labels joined
     * @param sep What joins the labels
     * @param indent The indent of each line
     * @param out Where the lines go
     */
    private static void groups(
        final Map<String, Integer> counted, final String sep, final String indent,
        final StringBuilder out
    ) {
        final List<Map.Entry<String, Integer>> found = new ArrayList<>(counted.entrySet());
        final CodeOrder order = new CodeOrder();
        found.sort(
            (left, right) -> {
                int cmp = Integer.compare(right.getValue(), left.getValue());
                if (cmp == 0) {
                    final String[] lefts = left.getKey().split(sep, -1);
                    final String[] rights = right.getKey().split(sep, -1);
                    for (int idx = 0; idx < Math.min(lefts.length, rights.length) && cmp == 0;
                        ++idx) {
                        cmp = order.compare(lefts[idx], rights[idx]);
                    }
                    if (cmp == 0) {
                        cmp = Integer.compare(lefts.length, rights.length);
                    }
                }
                return cmp;
            }
        );
        if (found.size() == 1) {
            for (final String label : found.get(0).getKey().split(sep, -1)) {
                out.append(indent).append(Answers.element(label)).append('\n');
            }
        } else {
            for (final Map.Entry<String, Integer> entry : found) {
                out.append(indent).append("<set copies=\"").append(entry.getValue())
                    .append("\">\n");
                for (final String label : entry.getKey().split(sep, -1)) {
                    out.append(indent).append("  ").append(Answers.element(label)).append('\n');
                }
                out.append(indent).append("</set>\n");
            }
        }
    }

    /**
     * The element of a label.
     *
     * @param label The label
     * @return The element
     */
    private static String element(final String label) {
        final String elem;
        if ("D".equals(label)) {
            elem = "<data/>";
        } else if ("T".equals(label)) {
            elem = "<terminator/>";
        } else if ("?can't-tell".equals(label)) {
            elem = "<unknown/>";
        } else if ("?open-void".equals(label)) {
            elem = "<open/>";
        } else if ("?open-void-atom".equals(label)) {
            elem = "<open by=\"java\"/>";
        } else if (Telling.ONLY_JAVA.equals(label)) {
            elem = "<fails name=\"ρ\" by=\"java\"/>";
        } else if (label.charAt(0) == '!') {
            elem = String.format("<fails name=\"%s\"/>", Answers.quoted(label.substring(1)));
        } else if (label.charAt(0) == '?') {
            throw new IllegalStateException(
                String.format("%s is a mark answers.xml has no element for", label)
            );
        } else {
            elem = String.format("<is loc=\"%s\"/>", Answers.quoted(label));
        }
        return elem;
    }

    /**
     * The text with {@code &}, {@code <}, {@code >} and {@code "} escaped.
     *
     * @param text The text
     * @return The escaped text
     */
    private static String quoted(final String text) {
        return text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
            .replace("\"", "&quot;");
    }

    /**
     * The number of copies of every set.
     *
     * @param counted The number of copies of each set
     * @return The sum
     */
    private static int sum(final Map<String, Integer> counted) {
        return Arrays.stream(counted.values().toArray(new Integer[0]))
            .mapToInt(Integer::intValue).sum();
    }
}
