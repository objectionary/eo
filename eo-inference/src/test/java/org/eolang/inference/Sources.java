/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.cactoos.io.InputOf;
import org.eolang.parser.Canonical;
import org.eolang.parser.EoSyntax;

/**
 * The EO sources of a small program, made into {@code facts.xml} the way
 * Maven makes it.
 *
 * <p>The parser writes each source with no indent and reads it back,
 * {@link Canonical} gets the dotted names of the files in subfolders, and
 * {@link Prepared} makes it ready; {@link Facts} then writes the facts of
 * the whole program. Bytes, number and string, each with one void
 * {@code @}, are added when the program does not write them, since the
 * engine shares one copy of each.</p>
 *
 * @since 0.76.0
 */
final class Sources {

    /**
     * The directory to work in.
     */
    private final Path dir;

    /**
     * The sources, by path.
     */
    private final Map<String, String> files;

    /**
     * Ctor.
     *
     * @param home The directory to work in
     * @param pairs Paths and texts of the sources, one after the other
     */
    Sources(final Path home, final String... pairs) {
        this(home, Sources.map(pairs));
    }

    /**
     * Ctor.
     *
     * @param home The directory to work in
     * @param sources The sources, by path
     */
    Sources(final Path home, final Map<String, String> sources) {
        this.dir = home;
        this.files = sources;
    }

    /**
     * The file {@code facts.xml} of the program.
     *
     * @return The file
     * @throws IOException If a file cannot be written
     */
    Path facts() throws IOException {
        final Map<String, String> all = new LinkedHashMap<>(this.files);
        all.putIfAbsent("bytes.eo", "[@] > bytes\n");
        all.putIfAbsent("number.eo", "[@] > number\n");
        all.putIfAbsent("string.eo", "[@] > string\n");
        final String objects = all.keySet().stream()
            .filter(name -> name.contains("/"))
            .map(name -> name.replaceAll("\\.eo$", "").replace('/', '.'))
            .distinct()
            .sorted()
            .collect(Collectors.joining(" "));
        for (final Map.Entry<String, String> source : all.entrySet()) {
            final Path target = this.dir.resolve("pre")
                .resolve(source.getKey().replaceAll("\\.eo$", ".xmir"));
            Files.createDirectories(target.getParent());
            Files.write(
                target,
                Sources.prepared(source.getValue(), objects).toString()
                    .getBytes(StandardCharsets.UTF_8)
            );
        }
        new Facts().follow(this.dir.resolve("pre"), this.dir.resolve("tables"));
        return this.dir.resolve("tables").resolve("facts.xml");
    }

    /**
     * The program, read back from its {@code facts.xml}.
     *
     * @return The program
     * @throws IOException If a file cannot be written or read
     */
    Program program() throws IOException {
        return new FactsXml(this.facts()).program();
    }

    /**
     * The state of a walk over the program, with the canon made and the
     * passes run.
     *
     * @return The state
     * @throws IOException If a file cannot be written or read
     */
    State settled() throws IOException {
        final State state = new State(this.program(), new Names());
        state.copies.seed();
        state.passes.settle();
        return state;
    }

    /**
     * The XMIR of one source, made ready.
     *
     * @param source The text of the source
     * @param objects The dotted names of the files in subfolders
     * @return The prepared XMIR
     * @throws IOException If the source cannot be parsed
     */
    private static XML prepared(final String source, final String objects) throws IOException {
        final StringWriter raw = new StringWriter();
        try {
            final Transformer writer = TransformerFactory.newInstance().newTransformer();
            writer.setOutputProperty(OutputKeys.INDENT, "no");
            writer.transform(
                new DOMSource(
                    new EoSyntax(new InputOf(source), UnaryOperator.identity()).parsed().inner()
                ),
                new StreamResult(raw)
            );
        } catch (final TransformerException ex) {
            throw new IOException("Can't write the parsed source", ex);
        }
        return new Prepared().apply(
            new XMLDocument(
                new XMLDocument(
                    new Canonical(objects).apply(new XMLDocument(raw.toString())).inner()
                ).toString()
            )
        );
    }

    /**
     * Paths and texts, one after the other, as a map in their order.
     *
     * @param pairs The paths and texts
     * @return The map
     */
    private static Map<String, String> map(final String... pairs) {
        final Map<String, String> sources = new LinkedHashMap<>(0);
        for (int idx = 0; idx < pairs.length; idx += 2) {
            sources.put(pairs[idx], pairs[idx + 1]);
        }
        return sources;
    }
}
