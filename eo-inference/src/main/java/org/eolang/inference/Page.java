/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import org.xembly.Directives;

/**
 * One source file, ready to be shown to somebody.
 *
 * <p>The parser keeps the whole text of a file in the XMIR, beside the objects
 * it read out of it, so a page can show the program as its author wrote it
 * rather than as a machine printed it back. Every object carries the line and
 * column it was written at, and the tables carry what we found out about it,
 * and this puts the two together: the source, line by line, with a mark on
 * every stretch of text we can say something about.</p>
 *
 * <p>The tally at the top is counted here too, from the same answers as the
 * marks. A page that says nine tenths green and then draws half the file red
 * would be worse than no page, and counting in one place is what stops
 * that.</p>
 *
 * @since 0.70.0
 */
final class Page {

    /**
     * Where one line of the listing ends and the next begins.
     */
    private static final Pattern BREAK = Pattern.compile("\\R");

    /**
     * The XMIR of the file.
     */
    private final XML xmir;

    /**
     * What the tables answered, by the locator of the object.
     */
    private final Map<String, Answer> answers;

    /**
     * Ctor.
     * @param file The XMIR of the file, as {@code pre-inference} left it
     * @param told What the tables answered, by the locator of the object
     */
    Page(final XML file, final Map<String, Answer> told) {
        this.xmir = file;
        this.answers = told;
    }

    /**
     * The page.
     * @param name The path of the source file, relative to the program
     * @return The directives
     */
    Directives directives(final String name) {
        final List<String> lines = this.lines();
        final Map<Integer, Collection<Written>> written = this.written();
        final Map<String, Integer> counted = this.counted();
        final Directives dirs = new Directives()
            .add("page")
            .attr("file", name)
            .attr("named", Integer.toString(counted.getOrDefault("named", 0)))
            .attr("rooted", Integer.toString(counted.getOrDefault("rooted", 0)))
            .attr("atom", Integer.toString(counted.getOrDefault("atom", 0)))
            .attr("unfilled", Integer.toString(counted.getOrDefault("unfilled", 0)))
            .attr("blank", Integer.toString(counted.getOrDefault("blank", 0)));
        for (int index = 0; index < lines.size(); index = index + 1) {
            dirs.add("line").attr("n", Integer.toString(index + 1));
            dirs.append(
                new Pieces(
                    lines.get(index),
                    written.getOrDefault(index + 1, new ArrayList<>(0))
                ).directives()
            );
            dirs.up();
        }
        return dirs.up();
    }

    private List<String> lines() {
        final List<String> found = new ArrayList<>(0);
        for (final String text : this.xmir.xpath("/object/listing/text()")) {
            for (final String line : Page.BREAK.split(text, -1)) {
                found.add(line);
            }
        }
        if (!found.isEmpty() && found.get(found.size() - 1).isEmpty()) {
            found.remove(found.size() - 1);
        }
        return found;
    }

    private Map<Integer, Collection<Written>> written() {
        final Map<Integer, Collection<Written>> found = new LinkedHashMap<>(0);
        for (final XML object : this.xmir.nodes("//o[@loc and @line and @pos]")) {
            final Noted noted = new Noted(object);
            final String loc = noted.says("loc");
            final Answer answer = this.answers.get(loc);
            if (answer != null) {
                found.computeIfAbsent(
                    Integer.parseInt(noted.says("line")),
                    key -> new ArrayList<>(1)
                ).add(
                    new Written(
                        loc, Integer.parseInt(noted.says("pos")), noted.says("name"), answer
                    )
                );
            }
        }
        return found;
    }

    private Map<String, Integer> counted() {
        final Map<String, Integer> found = new HashMap<>(5);
        for (final String loc : this.xmir.xpath("//o[@loc]/@loc")) {
            final Answer answer = this.answers.get(loc);
            if (answer != null) {
                found.merge(new Band(answer).name(), 1, Integer::sum);
            }
        }
        return found;
    }
}
