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

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.cactoos.io.ResourceOf;
import org.cactoos.set.SetOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.hamcrest.Description;
import org.hamcrest.MatcherAssert;
import org.hamcrest.TypeSafeMatcher;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.yaml.snakeyaml.Yaml;

/**
 * Test case for {@link OptimizeMojo}.
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class GmiMojoTest {

    @Test
    @Disabled
    void bigSlowTest() throws Exception {
        final StringBuilder program = new StringBuilder(1000);
        for (int idx = 0; idx < 40; ++idx) {
            for (int spc = 0; spc < idx; ++spc) {
                program.append("  ");
            }
            program.append("[x y z] > foo\n");
        }
        final XML graph = GmiMojoTest.toGraph(program.toString(), "**");
        MatcherAssert.assertThat(
            ".foo .foo",
            new GmiMojoTest.ExistsIn(graph)
        );
    }

    @ParameterizedTest
    @MethodSource("yamlPacks")
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testPacks(final String pack) throws Exception {
        final Map<String, Object> map = new Yaml().load(
            new TextOf(
                new ResourceOf(
                    String.format("org/eolang/maven/gmis/%s", pack)
                )
            ).asString()
        );
        Assumptions.assumeTrue(
            map.get("skip") == null,
            String.format("%s is skipped", pack)
        );
        final Object value = map.get("inclusion");
        String inclusion = "**";
        if (value != null) {
            inclusion = value.toString().substring(1, value.toString().length() - 1);
        }
        final XML graph = GmiMojoTest.toGraph(map.get("eo").toString(), inclusion);
        final Collection<Executable> assertions = new LinkedList<>();
        for (final String loc : (Iterable<String>) map.get("locators")) {
            assertions.add(
                () -> MatcherAssert.assertThat(
                    loc,
                    new GmiMojoTest.ExistsIn(graph)
                )
            );
        }
        assertions.add(
            () -> MatcherAssert.assertThat(
                graph,
                new GmiMojoTest.LicensePresents()
            )
        );
        Assertions.assertAll(assertions);
    }

    @SuppressWarnings("PMD.UnusedPrivateMethod")
    private static Collection<String> yamlPacks() {
        return GmiMojoTest.yamls("org/eolang/maven/gmis/", "");
    }

    private static Collection<String> yamls(final String path,
        final String prefix
    ) {
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
     * Convert EO source to Graph.
     *
     * @param code Code in EO
     * @param inclusion Value of gmiIncludes property
     * @return The graph
     * @throws IOException If fails
     */
    private static XML toGraph(final String code, final String inclusion) throws IOException {
        final Path temp = Files.createTempDirectory("eo");
        final Path src = temp.resolve("foo/main.eo");
        new Home(temp).save(code, temp.relativize(src));
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve("eo-foreign.json");
        Catalogs.INSTANCE.make(foreign)
            .add("foo.main")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, src.toString());
        new Moja<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("cache", temp.resolve("cache/parsed"))
            .with("foreignFormat", "csv")
            .execute();
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .execute();
        new Moja<>(GmiMojo.class)
            .with("generateGmiXmlFiles", true)
            .with("generateXemblyFiles", true)
            .with("generateGraphFiles", true)
            .with("generateDotFiles", true)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("gmiIncludes", new SetOf<>(inclusion))
            .execute();
        return new XMLDocument(
            target.resolve(
                String.format("%s/foo/main.gmi.graph.xml", GmiMojo.DIR)
            )
        );
    }

    /**
     * Matcher for a license in XML graph.
     *
     * @since 0.28.12
     */
    private static final class LicensePresents extends TypeSafeMatcher<XML> {

        @Override
        public void describeTo(final Description description) {
            description.appendText("License is required at the beginning of the file");
        }

        @Override
        public boolean matchesSafely(final XML item) {
            return item.toString().contains("This file was auto-generated by eolang-maven-plugin");
        }
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
        @SuppressWarnings("PMD.NPathComplexity")
        private void matches(final String item) {
            String vertex = "ν0";
            for (final String sub : item.split(" ")) {
                final XML node = this.graph.nodes(
                    String.format("/graph/v[@id='%s']", vertex)
                ).get(0);
                if (sub.charAt(0) == '.') {
                    final List<String> opts = node.xpath(
                        String.format(
                            "e[@title='%s']/@to",
                            sub.substring(1)
                        )
                    );
                    if (opts.isEmpty()) {
                        throw new IllegalArgumentException(
                            String.format(
                                "Can't find path '%s' while staying at %s",
                                sub, vertex
                            )
                        );
                    }
                    vertex = opts.get(0);
                    continue;
                }
                if (sub.startsWith("Δ=")) {
                    if (node.nodes("data").isEmpty()) {
                        throw new IllegalArgumentException(
                            String.format(
                                "There is no data (%s) at %s",
                                sub, vertex
                            )
                        );
                    }
                    final String data = sub.substring(2).replace('-', ' ');
                    final boolean matches = !node.xpath(
                        String.format(
                            "data[text() = '%s']/text()", data
                        )
                    ).isEmpty();
                    if (!matches) {
                        throw new IllegalArgumentException(
                            String.format(
                                "Data '%s' at '%s' is not equal to '%s'",
                                node.xpath("data/text()").get(0), vertex, data
                            )
                        );
                    }
                    continue;
                }
                if (sub.startsWith("λ=")) {
                    if (node.nodes("lambda").isEmpty()) {
                        throw new IllegalArgumentException(
                            String.format(
                                "There is no lambda (%s) at %s",
                                sub, vertex
                            )
                        );
                    }
                    final String expr = sub.substring(2);
                    final boolean matches = !this.graph.xpath(
                        String.format(
                            "/graph/v[@id='%s']/lambda[text() = '%s']/text()",
                            vertex, expr
                        )
                    ).isEmpty();
                    if (!matches) {
                        throw new IllegalArgumentException(
                            String.format(
                                "Lambda '%s' at '%s' is not equal to '%s'",
                                node.xpath("lambda/text()").get(0), vertex, expr
                            )
                        );
                    }
                    continue;
                }
                if (sub.startsWith("ν=")) {
                    final String expected = sub.substring(2);
                    final boolean matches = vertex.equals(expected);
                    if (!matches) {
                        throw new IllegalArgumentException(
                            String.format(
                                "Current vertex '%s' is not '%s', as expected",
                                vertex, expected
                            )
                        );
                    }
                }
                throw new IllegalArgumentException(
                    String.format(
                        "Can't understand path element '%s' in '%s'",
                        sub, item
                    )
                );
            }
        }

    }

}
