/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.tojos.Csv;
import com.yegor256.tojos.MonoTojos;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.hamcrest.Description;
import org.hamcrest.MatcherAssert;
import org.hamcrest.TypeSafeMatcher;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.xembly.Directives;
import org.xembly.Xembler;
import org.yaml.snakeyaml.Yaml;

/**
 * Test case for {@link OptimizeMojo}.
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public final class GmiMojoTest {

    @ParameterizedTest
    @MethodSource("yamlPacks")
    public void testPacks(final String pack) throws Exception {
        final Map<String, Object> map = new Yaml().load(
            new TextOf(
                new ResourceOf(
                    String.format("org/eolang/maven/gmis/%s", pack)
                )
            ).asString()
        );
        final String xembly = GmiMojoTest.toXembly(map.get("eo").toString());
        final XML graph = new XMLDocument(
            new Xembler(new Directives(xembly)).domQuietly()
        );
        Logger.info(this, "Graph:\n%s", graph);
        for (final String loc : (Iterable<String>) map.get("locators")) {
            MatcherAssert.assertThat(
                loc, new GmiMojoTest.ExistsIn(graph)
            );
        }
        for (final String xpath : (Iterable<String>) map.get("xpaths")) {
            MatcherAssert.assertThat(
                graph,
                XhtmlMatchers.hasXPath(xpath)
            );
        }
    }

    @SuppressWarnings("PMD.UnusedPrivateMethod")
    private static Collection<String> yamlPacks() {
        return GmiMojoTest.yamls("org/eolang/maven/gmis/", "");
    }

    private static Collection<String> yamls(final String path,
        final String prefix) {
        final Collection<String> out = new LinkedList<>();
        final String[] paths = new UncheckedText(
            new TextOf(new ResourceOf(path))
        ).asString().split("\n");
        for (final String sub : paths) {
            if (sub.endsWith(".yaml")) {
                out.add(String.format("%s%s", prefix, sub));
            } else {
                out.addAll(
                    GmiMojoTest.yamls(
                        String.format("%s%s/", path, sub),
                        String.format("%s/", sub)
                    )
                );
            }
        }
        return out;
    }

    /**
     * Convert EO source to Xembly instructions.
     * @param code Code in EO
     * @return Xembly code in plain text
     * @throws IOException If fails
     */
    private static String toXembly(final String code) throws IOException {
        final Path temp = Files.createTempDirectory("eo");
        final Path src = temp.resolve("foo/main.eo");
        new Save(code, src).save();
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve("eo-foreign.json");
        new MonoTojos(new Csv(foreign))
            .add("foo.main")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, src.toString());
        new Moja<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .execute();
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .execute();
        new Moja<>(GmiMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .execute();
        Logger.info(
            GmiMojoTest.class, "GMIs:\n  %s",
            new String(
                Files.readAllBytes(
                    target.resolve(
                        String.format("%s/foo/main.gmi", GmiMojo.DIR)
                    )
                ),
                StandardCharsets.UTF_8
            ).replace("\n", "\n  ")
        );
        return new String(
            Files.readAllBytes(
                target.resolve(
                    String.format("%s/foo/main.gmi.xe", GmiMojo.DIR)
                )
            ),
            StandardCharsets.UTF_8
        );
    }

    /**
     * Matcher for a single locator against the graph.
     *
     * @since 0.27
     */
    private static final class ExistsIn extends TypeSafeMatcher<String> {
        /**
         * Graph in XML.
         */
        private final XML graph;

        /**
         * The description of a failure.
         */
        private String failure;

        /**
         * Ctor.
         * @param xml The graph
         */
        ExistsIn(final XML xml) {
            super();
            this.graph = xml;
        }

        @Override
        public void describeTo(final Description desc) {
            desc.appendText(this.failure);
        }

        @Override
        public boolean matchesSafely(final String item) {
            boolean matches = true;
            String vertex = "v0";
            for (final String sub : item.split(" ")) {
                if (sub.charAt(0) == '.') {
                    final List<String> opts = this.graph.xpath(
                        String.format(
                            "//v[@id='%s']/e[@title='%s']/@to",
                            vertex, sub.substring(1)
                        )
                    );
                    if (opts.isEmpty()) {
                        this.failure = String.format(
                            "Can't find path '%s' while staying at %s",
                            sub, vertex
                        );
                        matches = false;
                        break;
                    }
                    vertex = opts.get(0);
                    continue;
                }
                if (sub.charAt(0) == '=') {
                    matches = !this.graph.xpath(
                        String.format(
                            "//v[@id='%s']/data[text() = '%s']/text()",
                            vertex, sub.substring(1)
                        )
                    ).isEmpty();
                    if (!matches) {
                        this.failure = String.format(
                            "Can't find data '%s' while staying at %s",
                            sub, vertex
                        );
                        break;
                    }
                }
            }
            return matches;
        }

    }

}
