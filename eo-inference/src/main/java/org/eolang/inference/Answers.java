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
import java.util.Set;
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
     * The end of every line, the same on every system.
     */
    private static final String EOL = String.valueOf('\n');

    /**
     * The elements of the labels that always read the same, by label.
     */
    private static final Map<String, String> FIXED = Map.of(
        "D", "<data/>",
        "T", "<terminator/>",
        "?can't-tell", "<unknown/>",
        "?open-void", "<open/>",
        "?open-void-atom", "<open by=\"java\"/>",
        Telling.ONLY_JAVA, "<fails name=\"ρ\" by=\"java\"/>"
    );

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
        final Set<String> places = new TreeSet<>(new CodeOrder());
        places.addAll(told.sets().keySet());
        places.addAll(told.reduction().keySet());
        places.addAll(told.lands().keySet());
        Files.createDirectories(file.toAbsolutePath().getParent());
        try (Writer out = Files.newBufferedWriter(file, StandardCharsets.UTF_8)) {
            out.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>").append(Answers.EOL)
                .append("<answers>").append(Answers.EOL);
            for (final String place : places) {
                out.write(Answers.place(told, place));
            }
            out.append("</answers>").append(Answers.EOL);
        }
    }

    private static String place(final Told told, final String place) {
        final Map<String, Integer> sets = told.sets().getOrDefault(
            place, Collections.emptyMap()
        );
        final StringBuilder line = new StringBuilder(128)
            .append("  <o loc=\"").append(Answers.quoted(place))
            .append("\" copies=\"").append(Answers.sum(sets)).append('"');
        if (told.lands().containsKey(place)) {
            line.append(" lands=\"").append(Answers.quoted(told.lands().get(place)))
                .append('"');
        }
        line.append('>').append(Answers.EOL);
        Answers.groups(sets, "\t", "    ", line);
        if (told.reduction().containsKey(place)) {
            final Map<String, Integer> reduced = told.reduction().get(place);
            line.append("    <reduced copies=\"").append(Answers.sum(reduced))
                .append("\">").append(Answers.EOL);
            Answers.groups(reduced, " ", "      ", line);
            line.append("    </reduced>").append(Answers.EOL);
        }
        return line.append("  </o>").append(Answers.EOL).toString();
    }

    private static void groups(
        final Map<String, Integer> counted, final String sep, final String indent,
        final StringBuilder out
    ) {
        final List<Map.Entry<String, Integer>> found = new ArrayList<>(counted.entrySet());
        final CodeOrder order = new CodeOrder();
        found.sort((left, right) -> Answers.compared(left, right, sep, order));
        if (found.size() == 1) {
            for (final String label : found.get(0).getKey().split(sep, -1)) {
                out.append(indent).append(Answers.element(label)).append(Answers.EOL);
            }
        } else {
            for (final Map.Entry<String, Integer> entry : found) {
                out.append(indent).append("<set copies=\"").append(entry.getValue())
                    .append("\">").append(Answers.EOL);
                for (final String label : entry.getKey().split(sep, -1)) {
                    out.append(indent).append("  ").append(Answers.element(label))
                        .append(Answers.EOL);
                }
                out.append(indent).append("</set>").append(Answers.EOL);
            }
        }
    }

    private static int compared(
        final Map.Entry<String, Integer> left, final Map.Entry<String, Integer> right,
        final String sep, final CodeOrder order
    ) {
        int cmp = Integer.compare(right.getValue(), left.getValue());
        if (cmp == 0) {
            final String[] lefts = left.getKey().split(sep, -1);
            final String[] rights = right.getKey().split(sep, -1);
            for (int idx = 0; idx < Math.min(lefts.length, rights.length) && cmp == 0; ++idx) {
                cmp = order.compare(lefts[idx], rights[idx]);
            }
            if (cmp == 0) {
                cmp = Integer.compare(lefts.length, rights.length);
            }
        }
        return cmp;
    }

    private static String element(final String label) {
        if (label.charAt(0) == '?' && !Answers.FIXED.containsKey(label)) {
            throw new IllegalStateException(
                String.format("%s is a mark answers.xml has no element for", label)
            );
        }
        final String elem;
        if (Answers.FIXED.containsKey(label)) {
            elem = Answers.FIXED.get(label);
        } else if (label.charAt(0) == '!') {
            elem = String.format("<fails name=\"%s\"/>", Answers.quoted(label.substring(1)));
        } else {
            elem = String.format("<is loc=\"%s\"/>", Answers.quoted(label));
        }
        return elem;
    }

    private static String quoted(final String text) {
        return text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
            .replace("\"", "&quot;");
    }

    private static int sum(final Map<String, Integer> counted) {
        return Arrays.stream(counted.values().toArray(new Integer[0]))
            .mapToInt(Integer::intValue).sum();
    }
}
