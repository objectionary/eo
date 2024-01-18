/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;
import org.cactoos.io.InputOf;
import org.cactoos.set.SetOf;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.xax.XaxStory;
import org.hamcrest.Description;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.TypeSafeMatcher;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.junit.jupiter.params.ParameterizedTest;
import org.yaml.snakeyaml.Yaml;

/**
 * Test case for {@link SodgMojo}.
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class SodgMojoTest {
    @Test
    @Disabled
    void convertsToGraph() throws Exception {
        final StringBuilder program = new StringBuilder(1000);
        for (int idx = 0; idx < 40; ++idx) {
            for (int spc = 0; spc < idx; ++spc) {
                program.append("  ");
            }
            program.append("[x y z] > foo\n");
        }
        final XML graph = SodgMojoTest.toGraph(program.toString(), "**");
        MatcherAssert.assertThat(
            ".foo .foo",
            new SodgMojoTest.ExistsIn(graph)
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/sodg-packs", glob = "**.yaml")
    void transformsThroughSheets(final String yaml) {
        MatcherAssert.assertThat(
            new XaxStory(yaml),
            Matchers.is(true)
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/sodgs/", glob = "**.yaml")
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void generatesSodgForPacks(final String pack) throws Exception {
        final Map<String, Object> map = new Yaml().load(pack);
        Assumptions.assumeTrue(
            map.get("skip") == null,
            String.format("%s is skipped", pack)
        );
        Object inclusion = map.get("inclusion");
        if (inclusion == null) {
            inclusion = "**";
        } else {
            inclusion = inclusion.toString().substring(
                1, inclusion.toString().length() - 1
            );
        }
        final XML graph = SodgMojoTest.toGraph(
            map.get("eo").toString(), inclusion.toString()
        );
        final Collection<Executable> assertions = new LinkedList<>();
        for (final String loc : (Iterable<String>) map.get("locators")) {
            assertions.add(
                () -> MatcherAssert.assertThat(
                    loc,
                    new SodgMojoTest.ExistsIn(graph)
                )
            );
        }
        Assertions.assertAll(assertions);
    }

    /**
     * Convert EO source to Graph.
     *
     * @param code Code in EO
     * @param inclusion Value of sodgIncludes property
     * @return The graph
     * @throws IOException If fails
     */
    private static XML toGraph(final String code, final String inclusion) throws Exception {
        final Map<String, Path> res = new FakeMaven(Files.createTempDirectory("eo"))
            .with("sodgIncludes", new SetOf<>(inclusion))
            .withProgram(code)
            .execute(new FakeMaven.Sodg())
            .result();
        Logger.debug(
            SodgMojoTest.class,
            "XML: %s",
            new TextOf(
                new InputOf(res.get(String.format("target/%s/foo/x/main.xmir", OptimizeMojo.DIR)))
            ).asString()
        );
        return new XMLDocument(
            res.get(String.format("target/%s/foo/x/main.sodg.graph.xml", SodgMojo.DIR))
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
            desc.appendText(this.failure)
                .appendText(" in this XML:\n")
                .appendText(this.graph.toString());
        }

        @Override
        public boolean matchesSafely(final String item) {
            boolean matches = true;
            try {
                this.matches(item);
            } catch (final IllegalArgumentException ex) {
                matches = false;
                this.failure = ex.getMessage();
            }
            return matches;
        }

        /**
         * Check and throw if fails.
         * @param item The path to check
         * @checkstyle CyclomaticComplexityCheck (10 lines)
         * @checkstyle NPathComplexityCheck (10 lines)
         */
        @SuppressWarnings({"PMD.NPathComplexity", "PMD.ExcessiveMethodLength"})
        private void matches(final String item) {
            String vertex = "ν0";
            final String[] parts = item.split(" ");
            for (int pos = 0; pos < parts.length; ++pos) {
                String sub = parts[pos];
                boolean inverse = false;
                final XML node = this.graph.nodes(
                    String.format("/graph/v[@id='%s']", vertex)
                ).get(0);
                if (sub.charAt(0) == '!') {
                    inverse = true;
                    sub = sub.substring(1);
                }
                if (sub.charAt(0) == '.') {
                    final List<String> opts = node.xpath(
                        String.format(
                            "e[@title='%s']/@to",
                            sub.substring(1)
                        )
                    );
                    if (opts.isEmpty() && !inverse) {
                        throw new IllegalArgumentException(
                            String.format(
                                "Can't find path '%s' (#%d) while staying at %s",
                                sub, pos, vertex
                            )
                        );
                    }
                    if (!opts.isEmpty() && inverse) {
                        throw new IllegalArgumentException(
                            String.format(
                                "The path '%s' (#%d) must not exist at %s, but it does",
                                sub, pos, vertex
                            )
                        );
                    }
                    if (!inverse) {
                        vertex = opts.get(0);
                    }
                    continue;
                }
                if (sub.charAt(0) == '>') {
                    final List<XML> inputs = this.graph.nodes(
                        String.format("/graph/v/e[@to='%s']", vertex)
                    );
                    if (inputs.isEmpty() && !inverse) {
                        throw new IllegalArgumentException(
                            String.format(
                                "There is no '%s' (#%d) edge coming into %s",
                                sub.substring(1), pos, vertex
                            )
                        );
                    }
                    continue;
                }
                if (sub.startsWith("δ=")) {
                    if (node.nodes("data").isEmpty()) {
                        throw new IllegalArgumentException(
                            String.format(
                                "There is no data (%s) at %s (#%d)",
                                sub, vertex, pos
                            )
                        );
                    }
                    final String data = sub.substring(2);
                    final boolean matches = !node.xpath(
                        String.format(
                            "data[text() = '%s']/text()", data
                        )
                    ).isEmpty();
                    if (!matches) {
                        throw new IllegalArgumentException(
                            String.format(
                                "Data '%s' at '%s' (#%d) is not equal to '%s'",
                                node.xpath("data/text()").get(0), vertex, pos, data
                            )
                        );
                    }
                    continue;
                }
                if (sub.startsWith("τ=")) {
                    if (node.nodes("data").isEmpty()) {
                        throw new IllegalArgumentException(
                            String.format(
                                "There is no lambda (%s) at %s (#%d)",
                                sub, vertex, pos
                            )
                        );
                    }
                    final String data = sub.substring(2);
                    final String hex = SodgMojoTest.ExistsIn.bytesToHex(
                        data.getBytes(StandardCharsets.UTF_8)
                    );
                    final boolean matches = !node.xpath(
                        String.format(
                            "data[text() = '%s']/text()",
                            hex
                        )
                    ).isEmpty();
                    if (!matches) {
                        throw new IllegalArgumentException(
                            String.format(
                                "Lambda '%s' at '%s' (#%d) is not equal to '%s' (%s)",
                                node.xpath("data/text()").get(0), vertex, pos, data, hex
                            )
                        );
                    }
                    continue;
                }
                if (sub.startsWith("ν=")) {
                    final String expected = sub.substring(2);
                    final boolean matches = vertex.equals(expected);
                    if (!matches && !inverse) {
                        throw new IllegalArgumentException(
                            String.format(
                                "Current vertex '%s' is not '%s' (#%d), as expected",
                                vertex, expected, pos
                            )
                        );
                    }
                    continue;
                }
                throw new IllegalArgumentException(
                    String.format(
                        "Can't understand path element '%s' (#%d) in '%s'",
                        sub, pos, item
                    )
                );
            }
        }

        /**
         * Bytes to HEX.
         * @param bytes Bytes.
         * @return Hexadecimal value as string.
         */
        private static String bytesToHex(final byte... bytes) {
            final StringJoiner out = new StringJoiner("-");
            for (final byte bty : bytes) {
                out.add(String.format("%02X", bty));
            }
            return out.toString();
        }
    }

}
