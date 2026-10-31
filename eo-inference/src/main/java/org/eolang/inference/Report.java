/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Xnav;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Templates;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import net.sf.saxon.TransformerFactoryImpl;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The coverage report of the inference: a page per source file, and an index.
 *
 * <p>{@code answers.xml} is true and unreadable. Nobody opens it to find out
 * why their object has no name, and a count per band says how much of a
 * program is named without saying which part. A page per file, with the
 * author's own source on it and a mark on every object coloured by its
 * {@link Band}, says both at once, and says them to a reader rather than to a
 * machine. The index lists the pages, folder by folder, with a bar per file
 * that adds up its objects by band.</p>
 *
 * <p>The pages are drawn byte for byte the way the Python report of the
 * engine draws them, from the same prepared XMIR and the same table, so the
 * two can be compared with {@code diff -r}. That is why the files are sorted
 * by code point, why the stylesheets are run by Saxon, which writes the bytes
 * {@code xsltproc} writes when told to put no {@code Content-Type} into the
 * head, and why every file ends with a line feed. The directory is emptied
 * first, so the page of a deleted source goes with it.</p>
 *
 * @since 0.70.0
 */
public final class Report {

    /**
     * The directory with the prepared XMIR files.
     */
    private final Path prepared;

    /**
     * The file {@code answers.xml}.
     */
    private final Path answers;

    /**
     * Ctor.
     *
     * @param xmirs The directory with the prepared XMIR files
     * @param table The file {@code answers.xml}
     */
    public Report(final Path xmirs, final Path table) {
        this.prepared = xmirs;
        this.answers = table;
    }

    /**
     * Write the pages and the index, in place of whatever the directory held.
     *
     * @param out The directory to write them into
     * @return How many objects of the program fall in each band, in the order
     *  a tally lists them
     * @throws IOException If a file cannot be read or written
     */
    public Map<String, Integer> written(final Path out) throws IOException {
        Report.wiped(out);
        Files.createDirectories(out);
        final Map<String, Xnav> roots = this.roots();
        final Map<String, Xnav> formations = new HashMap<>(0);
        final Map<String, String> files = new HashMap<>(0);
        for (final Map.Entry<String, Xnav> root : roots.entrySet()) {
            this.objects(root.getValue()).forEach(
                object -> {
                    final String loc = new Noted(object).says("loc");
                    if (!loc.isEmpty() && object.attribute("base").text().isEmpty()) {
                        formations.put(loc, object);
                        files.put(loc, root.getKey());
                    }
                }
            );
        }
        final Table table = new Table(new Xnav(this.answers));
        final Shapes shapes = new Shapes(formations, files, table);
        final Templates sheet = Report.sheet("page.xsl");
        final Map<String, Map<String, Integer>> tallies = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Xnav> root : roots.entrySet()) {
            final Document page = Report.document();
            final Element drawn = new Page(root.getValue(), root.getKey(), table, shapes)
                .drawn(page);
            page.appendChild(drawn);
            Report.styled(sheet, page, out.resolve(root.getKey().concat(".html")));
            tallies.put(root.getKey(), Report.tally(drawn));
        }
        final Document index = Report.document();
        final Element listed = Report.listed(index, tallies);
        index.appendChild(listed);
        Report.styled(Report.sheet("index.xsl"), index, out.resolve("index.html"));
        return Report.tally(listed);
    }

    private static void wiped(final Path out) throws IOException {
        if (Files.exists(out)) {
            final List<Path> all;
            try (Stream<Path> walked = Files.walk(out)) {
                all = walked.sorted(Comparator.reverseOrder()).toList();
            }
            for (final Path path : all) {
                if (path.toString().endsWith(".html")) {
                    Files.delete(path);
                }
            }
        }
    }

    private Map<String, Xnav> roots() throws IOException {
        final List<String> paths = new ArrayList<>(0);
        if (Files.exists(this.prepared)) {
            try (Stream<Path> walked = Files.walk(this.prepared)) {
                walked.filter(path -> path.toString().endsWith(".xmir"))
                    .filter(Files::isRegularFile)
                    .map(path -> this.prepared.relativize(path).toString().replace('\\', '/'))
                    .forEach(paths::add);
            }
        }
        paths.sort(new CodeOrder());
        final Map<String, Xnav> found = new LinkedHashMap<>(paths.size());
        for (final String path : paths) {
            found.put(
                path.substring(0, path.length() - ".xmir".length()).concat(".eo"),
                new Xnav(this.prepared.resolve(path))
            );
        }
        return found;
    }

    private Stream<Xnav> objects(final Xnav node) {
        return node.elements()
            .filter(kid -> kid.node().getNodeType() == Node.ELEMENT_NODE)
            .flatMap(this::within);
    }

    private Stream<Xnav> within(final Xnav kid) {
        return Stream.concat(
            Stream.of(kid).filter(object -> "o".equals(object.node().getNodeName())),
            this.objects(kid)
        );
    }

    private static Element listed(
        final Document index, final Map<String, Map<String, Integer>> pages
    ) {
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
        final Element found = index.createElement("index");
        Report.counted(found, new ArrayList<>(pages.keySet()), pages);
        final List<String> sorted = new ArrayList<>(folders.keySet());
        sorted.sort(new CodeOrder());
        for (final String folder : sorted) {
            if (!folder.isEmpty()) {
                final Element dir = index.createElement("dir");
                dir.setAttribute("name", folder);
                Report.counted(dir, folders.get(folder), pages);
                Report.rows(dir, folders.get(folder), pages);
                found.appendChild(dir);
            }
        }
        Report.rows(found, folders.getOrDefault("", new ArrayList<>(0)), pages);
        return found;
    }

    private static void rows(
        final Element parent, final List<String> names,
        final Map<String, Map<String, Integer>> pages
    ) {
        for (final String name : names) {
            final Element file = parent.getOwnerDocument().createElement("file");
            file.setAttribute("name", name.substring(name.lastIndexOf('/') + 1));
            file.setAttribute("href", name.concat(".html"));
            for (final String band : Band.ORDER) {
                file.setAttribute(band, Integer.toString(pages.get(name).get(band)));
            }
            parent.appendChild(file);
        }
    }

    private static void counted(
        final Element parent, final List<String> names,
        final Map<String, Map<String, Integer>> pages
    ) {
        for (final String band : Band.ORDER) {
            parent.setAttribute(
                band,
                Integer.toString(names.stream().mapToInt(name -> pages.get(name).get(band)).sum())
            );
        }
    }

    private static Map<String, Integer> tally(final Element counted) {
        final Map<String, Integer> found = new LinkedHashMap<>(Band.ORDER.size());
        for (final String band : Band.ORDER) {
            found.put(band, Integer.parseInt(counted.getAttribute(band)));
        }
        return found;
    }

    private static Document document() {
        try {
            return DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
        } catch (final ParserConfigurationException ex) {
            throw new IllegalStateException("Can't make an empty document for a page", ex);
        }
    }

    private static Templates sheet(final String name) {
        final String path = String.format("/org/eolang/inference/report/%s", name);
        try {
            return new TransformerFactoryImpl().newTemplates(
                new StreamSource(Report.class.getResource(path).toExternalForm())
            );
        } catch (final TransformerConfigurationException ex) {
            throw new IllegalStateException(
                String.format("Can't compile the stylesheet %s", path), ex
            );
        }
    }

    private static void styled(final Templates sheet, final Document page, final Path target)
        throws IOException {
        final ByteArrayOutputStream html = new ByteArrayOutputStream();
        try {
            sheet.newTransformer().transform(new DOMSource(page), new StreamResult(html));
        } catch (final TransformerException ex) {
            throw new IllegalStateException(
                String.format("Can't draw the page %s", target), ex
            );
        }
        html.write('\n');
        Files.createDirectories(target.getParent());
        Files.write(target, html.toByteArray());
    }
}
