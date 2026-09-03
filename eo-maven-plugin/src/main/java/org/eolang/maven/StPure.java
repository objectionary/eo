/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.ClasspathSources;
import com.jcabi.xml.Sources;
import com.jcabi.xml.XSL;
import com.jcabi.xml.XSLDocument;
import com.yegor256.xsline.StEnvelope;
import com.yegor256.xsline.StXSL;
import java.io.IOException;
import java.lang.ref.SoftReference;
import java.net.URI;
import java.nio.file.FileSystemNotFoundException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

/**
 * The step of the transpile train that runs {@code purify.xsl}, which marks
 * the formations that are safe to cache by reading the tables
 * {@link MjInference} leaves on disk.
 *
 * <p>It is not a {@link com.yegor256.xsline.StClasspath} only because of
 * where those tables are. A stylesheet reads another document through the
 * resolver of its own transformation, and the one that
 * {@link com.yegor256.xsline.StClasspath} installs looks for every document
 * on the classpath, where the tables of a build under way are not. So the
 * resolver here answers a {@code file:} URI from the disk and leaves
 * everything else — the libraries a stylesheet {@code xsl:import}-s — to the
 * classpath, as before. A table that is not there stays not there: the
 * resolver says so, {@code doc-available()} in the stylesheet hears it, and a
 * build that skips {@code eo:inference} marks nothing.</p>
 *
 * <p>A table read once is kept, because the stylesheet asks for it again for
 * every XMIR it stamps and the tables of {@code eo-runtime} are tens of
 * megabytes — a second per file when every ask parses them afresh. One copy
 * is kept instead, read by every thread of {@link Threaded} and written by
 * none, and parsed with node expansion turned off so that no reader builds a
 * part of it while another one walks past. It is held by a soft reference,
 * since those megabytes are hundreds once parsed and reading a table again is
 * better than running a build out of memory.</p>
 *
 * <p>What is kept is one snapshot of a table, not the name of its file. The
 * same Maven process runs {@code eo:inference} and then transpiles more than
 * once, and the second inference writes its tables to the same paths, so a
 * copy kept by URI alone would answer the second transpilation with what the
 * first one read, and the Java it generates would be marked by types that
 * have moved on. The modification time and the length of the file are kept
 * beside the copy and asked again before it is handed over.</p>
 *
 * @since 0.75.0
 */
final class StPure extends StEnvelope {

    /**
     * The tables read so far, by their URIs.
     */
    private static final Map<String, SoftReference<Node>> TABLES = new HashMap<>(0);

    /**
     * What each kept table looked like on disk when it was read, by URI.
     */
    private static final Map<String, String> STAMPS = new HashMap<>(0);

    /**
     * The lock on {@link #TABLES}, held while a table is being read, so that
     * the threads that want the same one wait for it instead of each reading
     * a copy of its own.
     */
    private static final Lock LOCK = new ReentrantLock();

    /**
     * Ctor.
     * @param sheet The classpath path of the stylesheet to run
     * @param tables The directory with the tables of {@link MjInference},
     *  which does not have to exist
     */
    StPure(final String sheet, final Path tables) {
        super(new StXSL(StPure.compiled(sheet, tables)));
    }

    private static XSL compiled(final String sheet, final Path tables) {
        try {
            return new XSLDocument(StPure.class.getResource(sheet), sheet)
                .with(StPure.sources())
                .with("inference", tables.toUri().toString());
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format("Failed to read '%s' from classpath", sheet), ex
            );
        }
    }

    private static Sources sources() {
        final Sources classpath = new ClasspathSources();
        return (href, base) -> {
            final Source found;
            if (href.startsWith("file:")) {
                found = new DOMSource(StPure.table(href), href);
            } else {
                found = classpath.resolve(href, base);
            }
            return found;
        };
    }

    private static Node table(final String href) throws TransformerException {
        StPure.LOCK.lock();
        try {
            final String stamp = StPure.stamp(href);
            final SoftReference<Node> kept = StPure.TABLES.get(href);
            Node found = null;
            if (kept != null && stamp.equals(StPure.STAMPS.get(href))) {
                found = kept.get();
            }
            if (found == null) {
                found = StPure.parsed(href);
                StPure.TABLES.put(href, new SoftReference<>(found));
                StPure.STAMPS.put(href, stamp);
            }
            return found;
        } finally {
            StPure.LOCK.unlock();
        }
    }

    private static String stamp(final String href) {
        String stamp;
        try {
            final BasicFileAttributes attrs = Files.readAttributes(
                Paths.get(URI.create(href)), BasicFileAttributes.class
            );
            stamp = String.format("%s %d", attrs.lastModifiedTime(), attrs.size());
        } catch (final IOException | IllegalArgumentException
            | FileSystemNotFoundException ex) {
            stamp = "";
        }
        return stamp;
    }

    private static Node parsed(final String href) throws TransformerException {
        try {
            final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            factory.setFeature("http://apache.org/xml/features/dom/defer-node-expansion", false);
            return factory.newDocumentBuilder().parse(href);
        } catch (final ParserConfigurationException | SAXException | IOException ex) {
            throw new TransformerException(
                String.format("Failed to read the table of types from '%s'", href), ex
            );
        }
    }
}
