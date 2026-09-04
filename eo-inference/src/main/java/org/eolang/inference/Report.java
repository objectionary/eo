/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSLDocument;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * A page per source file, for somebody to look at.
 *
 * <p>The tables are true and unreadable. Nobody opens {@code links.xml} to
 * find out why their object has no type, and the three numbers the goal prints
 * say how much of a program we understand without saying which part. A page
 * per file, with the author's own source on it and a mark on everything we can
 * name, says both at once, and says them to a reader rather than to a
 * machine.</p>
 *
 * <p>Green is a formation we can name, amber is a name rooted in somebody
 * else's void, red is nothing. The colours are the bands the goal prints, from
 * the same {@link Answered} the goal counts, so a page and a number cannot
 * disagree.</p>
 *
 * @since 0.70.0
 */
public final class Report {

    /**
     * The directory with the prepared XMIR files.
     */
    private final Path world;

    /**
     * The directory with the tables.
     */
    private final Path tables;

    /**
     * Ctor.
     * @param xmirs The directory with the prepared XMIR files
     * @param rows The directory with the tables
     */
    public Report(final Path xmirs, final Path rows) {
        this.world = xmirs;
        this.tables = rows;
    }

    /**
     * Write the pages.
     * @param out The directory to write them into
     * @return How many pages were written
     * @throws IOException If a table or a file cannot be read or written
     */
    public int written(final Path out) throws IOException {
        Files.createDirectories(out);
        final Map<String, Answer> told = new Answered(this.world, this.tables).all();
        final XSLDocument page = new XSLDocument(Report.stylesheet("page.xsl"));
        final Map<String, XML> pages = new LinkedHashMap<>(0);
        for (final Path file : this.sources()) {
            final String name = this.named(file);
            pages.put(
                name,
                new XMLDocument(
                    new Xembler(
                        new Page(new XMLDocument(file), told).directives(name)
                    ).xmlQuietly()
                )
            );
        }
        for (final Map.Entry<String, XML> made : pages.entrySet()) {
            final Path target = out.resolve(made.getKey().concat(".html"));
            Files.createDirectories(target.getParent());
            Files.write(
                target,
                page.applyTo(Report.rooted(made.getValue(), made.getKey()))
                    .getBytes(StandardCharsets.UTF_8)
            );
        }
        Files.write(
            out.resolve("index.html"),
            new XSLDocument(Report.stylesheet("index.xsl")).applyTo(
                new XMLDocument(
                    new Xembler(Report.listed(pages)).xmlQuietly()
                )
            ).getBytes(StandardCharsets.UTF_8)
        );
        return pages.size();
    }

    private List<Path> sources() throws IOException {
        final List<Path> found = new ArrayList<>(0);
        if (Files.exists(this.world)) {
            try (Stream<Path> walked = Files.walk(this.world)) {
                walked.filter(path -> path.toString().endsWith(".xmir"))
                    .filter(Files::isRegularFile)
                    .sorted()
                    .forEach(found::add);
            }
        }
        return found;
    }

    private String named(final Path file) {
        final String path = this.world.relativize(file).toString().replace('\\', '/');
        return path.substring(0, path.length() - ".xmir".length()).concat(".eo");
    }

    private static String stylesheet(final String name) {
        return new UncheckedText(
            new TextOf(
                new ResourceOf(
                    String.format("org/eolang/inference/report/%s", name),
                    Report.class
                )
            )
        ).asString();
    }

    private static XML rooted(final XML made, final String name) {
        final StringBuilder back = new StringBuilder(16);
        for (int step = 0; step < name.split("/").length - 1; step = step + 1) {
            back.append("../");
        }
        return new XMLDocument(
            new Xembler(
                new Directives().xpath("/page").attr("root", back.toString())
            ).applyQuietly(made.inner())
        );
    }

    private static Directives listed(final Map<String, XML> pages) {
        final Map<String, List<String>> folders = new LinkedHashMap<>(0);
        for (final String name : pages.keySet()) {
            final int slash = name.lastIndexOf('/');
            final String folder;
            if (slash < 0) {
                folder = "";
            } else {
                folder = name.substring(0, slash);
            }
            folders.computeIfAbsent(folder, key -> new ArrayList<>(1)).add(name);
        }
        final List<String> ordered = new ArrayList<>(folders.keySet());
        Collections.sort(ordered);
        final Directives dirs = new Directives().add("index");
        Report.summed(dirs, pages.values());
        for (final String folder : ordered) {
            if (!folder.isEmpty()) {
                dirs.add("dir").attr("name", folder);
                Report.rows(dirs, folders.get(folder), pages);
                dirs.up();
            }
        }
        Report.rows(dirs, folders.getOrDefault("", Collections.emptyList()), pages);
        return dirs.up();
    }

    private static void rows(
        final Directives dirs, final List<String> names, final Map<String, XML> pages
    ) {
        for (final String name : names) {
            final XML made = pages.get(name);
            dirs.add("file")
                .attr("name", name.substring(name.lastIndexOf('/') + 1))
                .attr("href", name.concat(".html"))
                .attr("named", made.xpath("/page/@named").get(0))
                .attr("rooted", made.xpath("/page/@rooted").get(0))
                .attr("blank", made.xpath("/page/@blank").get(0))
                .up();
        }
    }

    private static void summed(final Directives dirs, final Iterable<XML> pages) {
        int named = 0;
        int rooted = 0;
        int blank = 0;
        for (final XML made : pages) {
            named = named + Integer.parseInt(made.xpath("/page/@named").get(0));
            rooted = rooted + Integer.parseInt(made.xpath("/page/@rooted").get(0));
            blank = blank + Integer.parseInt(made.xpath("/page/@blank").get(0));
        }
        dirs.attr("named", Integer.toString(named))
            .attr("rooted", Integer.toString(rooted))
            .attr("blank", Integer.toString(blank));
    }
}
