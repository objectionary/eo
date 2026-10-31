/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.UnaryOperator;
import org.cactoos.Scalar;
import org.cactoos.io.InputOf;
import org.cactoos.io.ResourceOf;
import org.cactoos.list.ListOf;
import org.cactoos.text.IoCheckedText;
import org.cactoos.text.TextOf;
import org.eolang.inference.Answers;
import org.eolang.inference.Facts;
import org.eolang.inference.Names;
import org.eolang.inference.Prepared;
import org.eolang.inference.Walk;
import org.eolang.parser.Canonical;
import org.eolang.parser.EoSyntax;

/**
 * The tables of {@code eo:inference} for a small program, as the engine
 * itself writes them.
 *
 * <p>Every source is parsed and made ready by {@link Prepared}, and
 * {@link Facts} reads them all into {@code facts.xml}, which the engine,
 * {@link Walk}, reads to write {@code answers.xml} beside it, the way the
 * goal {@code eo:inference} does in a build, with the names the
 * {@code pom.xml} of eo-runtime gives it. So a test of the lowering
 * reads what the engine says of its program, and not what somebody once
 * thought the engine would say.</p>
 *
 * <p>A few objects of eo-runtime are added when the program does not write
 * them, from {@code org/eolang/lowering/runtime}, the way eo-runtime is
 * around every program of a build. The engine shares one copy of
 * {@code bytes}, {@code number} and {@code string}, each with nothing but
 * a void {@code @}, which is all a datum needs. And {@code bool},
 * {@code true} and {@code false} are written as eo-runtime writes them,
 * since only through them can the engine tell what an {@code if} gives
 * when a bool an atom returns takes it.</p>
 *
 * @since 0.64.0
 */
final class Inferred implements Scalar<Path> {

    /**
     * The directory to work in.
     */
    private final Path home;

    /**
     * The EO sources of the program, by file name.
     */
    private final Map<String, String> sources;

    /**
     * Ctor.
     *
     * @param dir The directory to work in
     * @param eos The EO sources of the program, by file name
     */
    Inferred(final Path dir, final Map<String, String> eos) {
        this.home = dir;
        this.sources = eos;
    }

    @Override
    public Path value() throws IOException {
        final Map<String, String> all = new TreeMap<>(this.sources);
        for (final String name : new ListOf<>(
            "bool.eo", "bytes.eo", "false.eo", "number.eo", "string.eo", "true.eo"
        )) {
            if (!all.containsKey(name)) {
                all.put(
                    name,
                    new IoCheckedText(
                        new TextOf(
                            new ResourceOf(String.format("org/eolang/lowering/runtime/%s", name))
                        )
                    ).asString()
                );
            }
        }
        final Path pre = this.home.resolve("pre-inference");
        final UnaryOperator<XML> canonical = new Canonical();
        final UnaryOperator<XML> prepared = new Prepared();
        for (final Map.Entry<String, String> source : all.entrySet()) {
            final Path xmir = pre.resolve(source.getKey().replaceAll("\\.eo$", ".xmir"));
            Files.createDirectories(xmir.getParent());
            Files.write(
                xmir,
                prepared.apply(
                    new XMLDocument(
                        new EoSyntax(new InputOf(source.getValue()), canonical).parsed()
                            .toString()
                    )
                ).toString().getBytes(StandardCharsets.UTF_8)
            );
        }
        final Path tables = this.home.resolve("inference");
        new Facts().follow(pre, tables);
        new Answers(
            new Walk(
                tables.resolve("facts.xml"),
                new Names(
                    List.of("Φ.bytes", "Φ.number", "Φ.string"),
                    List.of("Φ.tuple", "Φ.map", "Φ.set"),
                    List.of(
                        "Φ.number", "Φ.bytes", "Φ.string", "Φ.bool", "Φ.true", "Φ.false",
                        "Φ.i8", "Φ.i16", "Φ.i32", "Φ.i64", "Φ.u8", "Φ.u16", "Φ.u32", "Φ.u64",
                        "Φ.dataized"
                    )
                )
            )
        ).write(tables.resolve("answers.xml"));
        return tables;
    }
}
