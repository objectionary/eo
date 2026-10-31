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
import java.util.List;
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
 * <p>The engine walks the program with the names eo-runtime gives it in its
 * {@code pom.xml}, and this is the one place the tests write them.</p>
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
     * The names the engine has to know.
     */
    private final Names names;

    /**
     * Ctor, with no sources.
     *
     * @param home The directory to work in
     */
    Sources(final Path home) {
        this(home, new LinkedHashMap<>(0));
    }

    /**
     * Ctor, with the names of eo-runtime.
     *
     * @param home The directory to work in
     * @param sources The sources, by path
     */
    Sources(final Path home, final Map<String, String> sources) {
        this(
            home,
            sources,
            new Names(
                List.of("Φ.bytes", "Φ.number", "Φ.string"),
                List.of("Φ.tuple", "Φ.map", "Φ.set"),
                List.of(
                    "Φ.number", "Φ.bytes", "Φ.string", "Φ.bool", "Φ.true", "Φ.false",
                    "Φ.i8", "Φ.i16", "Φ.i32", "Φ.i64", "Φ.u8", "Φ.u16", "Φ.u32", "Φ.u64",
                    "Φ.dataized"
                )
            )
        );
    }

    /**
     * Ctor.
     *
     * @param home The directory to work in
     * @param sources The sources, by path
     * @param known The names the engine has to know
     */
    private Sources(final Path home, final Map<String, String> sources, final Names known) {
        this.dir = home;
        this.files = sources;
        this.names = known;
    }

    /**
     * The same sources and one more.
     *
     * @param path The path of the source
     * @param lines Its lines, each of them ended with a line feed
     * @return The sources
     */
    Sources with(final String path, final String... lines) {
        final Map<String, String> more = new LinkedHashMap<>(this.files);
        more.put(path, Sources.text(lines));
        return new Sources(this.dir, more, this.names);
    }

    /**
     * The file {@code facts.xml} of the program.
     *
     * @return The file
     * @throws IOException If a file cannot be written
     */
    Path facts() throws IOException {
        final Map<String, String> all = new LinkedHashMap<>(this.files);
        all.putIfAbsent("bytes.eo", Sources.text("[@] > bytes"));
        all.putIfAbsent("number.eo", Sources.text("[@] > number"));
        all.putIfAbsent("string.eo", Sources.text("[@] > string"));
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
     * The names the engine has to know.
     *
     * @return The names
     */
    Names names() {
        return this.names;
    }

    /**
     * The world a walk over the program goes over.
     *
     * @return The world
     * @throws IOException If a file cannot be written or read
     */
    World world() throws IOException {
        return new World(this.program(), this.names);
    }

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

    private static String text(final String... lines) {
        final StringBuilder text = new StringBuilder(64);
        for (final String line : lines) {
            text.append(line).append('\n');
        }
        return text.toString();
    }
}
