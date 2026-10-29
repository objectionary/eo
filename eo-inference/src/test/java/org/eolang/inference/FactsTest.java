/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
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
import org.eolang.jucs.ClasspathSource;
import org.eolang.parser.Canonical;
import org.eolang.parser.EoSyntax;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link Facts}.
 *
 * <p>What the facts say about a program is checked by the packs in
 * {@code packs}: each one carries the EO sources of a small program, the
 * {@code facts.xml} written of it, and the {@code answers.xml} the engine
 * gives on those facts. The expected text of both comes from the Python
 * engine and its own copy of these classes, so a pack is also a check, byte
 * for byte, that the Java here writes what the Python side reads. A source
 * goes through the steps Maven takes: the parser writes it with no indent
 * and reads it back, {@link Canonical} gets the dotted names of the files in
 * subfolders, and {@link Prepared} makes it ready.</p>
 *
 * @since 0.76.0
 * @todo #9650:90min Check the answers of every pack as well, once the
 *  engine is ported from engine.py. Each pack already carries the
 *  answers.xml that engine.py wrote from its facts, under the key
 *  "answers", and the port must write the same bytes from the facts this
 *  test writes. Add that check here, or in the test of the engine.
 */
@ExtendWith(MktmpResolver.class)
final class FactsTest {

    /**
     * Temp directory, injected into every test instance, since a
     * parameterized test cannot also take one as an argument.
     */
    @Mktmp
    private Path dir;

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/inference/packs/", glob = "**.yaml")
    void writesFactsOfPack(final String yaml) throws Exception {
        final Map<String, Object> pack = new XtSticky(new XtYaml(yaml)).map();
        final Map<String, String> sources = FactsTest.sources(pack);
        final String objects = sources.keySet().stream()
            .filter(name -> name.contains("/"))
            .map(name -> name.replaceAll("\\.eo$", "").replace('/', '.'))
            .distinct()
            .sorted()
            .collect(Collectors.joining(" "));
        for (final Map.Entry<String, String> source : sources.entrySet()) {
            final Path target = this.dir.resolve("pre")
                .resolve(source.getKey().replaceAll("\\.eo$", ".xmir"));
            Files.createDirectories(target.getParent());
            Files.write(
                target,
                FactsTest.prepared(source.getValue(), objects).toString()
                    .getBytes(StandardCharsets.UTF_8)
            );
        }
        new Facts().follow(this.dir.resolve("pre"), this.dir.resolve("tables"));
        MatcherAssert.assertThat(
            "the facts written of the program must be the facts of the pack, byte for byte, but they werent",
            new String(
                Files.readAllBytes(this.dir.resolve("tables").resolve("facts.xml")),
                StandardCharsets.UTF_8
            ),
            Matchers.equalTo(pack.get("facts").toString())
        );
    }

    private static XML prepared(final String source, final String objects)
        throws IOException, TransformerException {
        final Transformer writer = TransformerFactory.newInstance().newTransformer();
        writer.setOutputProperty(OutputKeys.INDENT, "no");
        final StringWriter raw = new StringWriter();
        writer.transform(
            new DOMSource(
                new EoSyntax(new InputOf(source), UnaryOperator.identity()).parsed().inner()
            ),
            new StreamResult(raw)
        );
        return new Prepared().apply(
            new XMLDocument(
                new XMLDocument(
                    new Canonical(objects).apply(new XMLDocument(raw.toString())).inner()
                ).toString()
            )
        );
    }

    private static Map<String, String> sources(final Map<String, Object> pack) {
        return ((Map<?, ?>) pack.get("sources")).entrySet().stream().collect(
            Collectors.toMap(
                entry -> entry.getKey().toString(),
                entry -> entry.getValue().toString()
            )
        );
    }
}
