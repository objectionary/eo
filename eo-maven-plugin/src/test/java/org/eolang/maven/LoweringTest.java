/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;
import org.eolang.jucs.ClasspathSource;
import org.eolang.lowering.Digest;
import org.eolang.lowering.Phino;
import org.eolang.lowering.Protocol;
import org.eolang.lowering.Reduction;
import org.eolang.lowering.Step;
import org.eolang.printer.Xmir;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.Xtory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link Lowering}.
 *
 * <p>Each YAML pack tells the whole story of one fragment: the EO
 * source it starts from, the EO it prints as after the lowering step,
 * and the protocol its symbolic reduction settles into — or the reason
 * it refuses. The packs are the visual record of what lowering does,
 * so a change in folding, printing, or reduction shows up here as a
 * readable diff. They hold only when a phino binary of the pinned
 * version is installed, which is what CI arranges.</p>
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class LoweringTest {

    /**
     * Temp directory, injected into every test instance.
     */
    @Mktmp
    private Path temp;

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/lowering-packs/", glob = "**.yaml")
    void printsLoweredEo(final String yaml) throws IOException {
        Assumptions.assumeTrue(new Phino("phino", 1000, this.temp).suitable());
        final Xtory story = new XtSticky(new XtYaml(yaml));
        MatcherAssert.assertThat(
            "the lowered XMIR must print as the pack promises, but it doesnt",
            new Xmir(
                new XMLDocument(
                    LoweringTest.maven(this.temp, story).execute(new PpLower())
                        .foreignTojos().find("foo").xmir()
                )
            ).toEO().trim(),
            Matchers.equalTo(story.map().get("lowered").toString().trim())
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/lowering-packs/", glob = "**.yaml")
    void reducesToProtocol(final String yaml) throws IOException {
        final Xtory story = new XtSticky(new XtYaml(yaml));
        Assumptions.assumeTrue(story.map().containsKey("protocol"));
        final Phino phino = new Phino("phino", 1000, this.temp);
        Assumptions.assumeTrue(phino.suitable());
        final Xnav foo = LoweringTest.formation(this.temp, story);
        final Map<String, String> voids = LoweringTest.voids(foo, story);
        MatcherAssert.assertThat(
            "the fragment must reduce into the protocol the pack promises, but it doesnt",
            LoweringTest.printed(
                new Reduction(
                    phino,
                    LoweringTest.fragment(foo),
                    voids,
                    8,
                    story.map().getOrDefault("unit", "").toString(),
                    LoweringTest.helpers(foo)
                ).protocol(),
                voids.size()
            ).trim(),
            Matchers.equalTo(story.map().get("protocol").toString().trim())
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/lowering-packs/", glob = "**.yaml")
    void refusesToReduce(final String yaml) throws IOException {
        final Xtory story = new XtSticky(new XtYaml(yaml));
        Assumptions.assumeTrue(story.map().containsKey("stuck"));
        final Phino phino = new Phino("phino", 1000, this.temp);
        Assumptions.assumeTrue(phino.suitable());
        final Xnav foo = LoweringTest.formation(this.temp, story);
        MatcherAssert.assertThat(
            "the refusal must say why the fragment stays unlowered, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                new Reduction(
                    phino,
                    LoweringTest.fragment(foo),
                    LoweringTest.voids(foo, story),
                    8,
                    story.map().getOrDefault("unit", "").toString(),
                    LoweringTest.helpers(foo)
                )::protocol,
                "a fragment the pack calls stuck cannot reduce, but it did"
            ).getMessage(),
            Matchers.containsString(story.map().get("stuck").toString())
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/lowering-packs/", glob = "**.yaml")
    void writesSidecarBody(final String yaml) throws IOException {
        final Xtory story = new XtSticky(new XtYaml(yaml));
        Assumptions.assumeTrue(story.map().containsKey("java"));
        Assumptions.assumeTrue(new Phino("phino", 1000, this.temp).suitable());
        final String body = story.map().get("java").toString().stripTrailing().lines()
            .map(line -> String.format("        %s", line))
            .collect(Collectors.joining(System.lineSeparator()));
        MatcherAssert.assertThat(
            "the sidecar must hold the Java body the pack promises, but it doesnt",
            Files.readString(
                LoweringTest.atoms(this.temp, story).resolve(
                    String.format("%s.java", new Digest(body).hex())
                )
            ),
            Matchers.equalTo(body)
        );
    }

    private static Path atoms(final Path temp, final Xtory story) throws IOException {
        return LoweringTest.maven(temp, story).execute(new PpLower())
            .targetPath().resolve("4-lower").resolve("atoms");
    }

    private static FakeMaven maven(final Path temp, final Xtory story) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(story.map().get("input").toString(), "foo", "foo.eo");
        final Object prelude = story.map().getOrDefault("prelude", Collections.emptyMap());
        for (final Map.Entry<?, ?> entry : ((Map<?, ?>) prelude).entrySet()) {
            maven.withProgram(
                entry.getValue().toString(),
                entry.getKey().toString().replace(".eo", ""),
                entry.getKey().toString()
            );
        }
        return maven;
    }

    private static Xnav formation(final Path temp, final Xtory story) throws IOException {
        Xnav found = new Xnav(
            new XMLDocument(
                LoweringTest.maven(temp, story).execute(MjParse.class)
                    .targetPath().resolve("1-parse").resolve("foo.xmir")
            ).inner()
        ).element("object").element("o");
        final Object unit = story.map().get("unit");
        if (unit != null) {
            found = found.elements(Filter.withName("o"))
                .filter(kid -> unit.toString().equals(kid.attribute("name").text().orElse("")))
                .findFirst().orElseThrow(
                    () -> new IllegalStateException(
                        String.format("The program binds no '%s'", unit)
                    )
                );
        }
        return found;
    }

    private static Xnav fragment(final Xnav formation) {
        return formation.elements(Filter.withName("o"))
            .filter(kid -> "φ".equals(kid.attribute("name").text().orElse("")))
            .findFirst()
            .orElseThrow(() -> new IllegalStateException("The formation binds no φ"));
    }

    private static Map<String, String> voids(final Xnav formation, final Xtory story) {
        final Object formas = story.map().getOrDefault(
            "voids", Collections.emptyMap()
        );
        final Map<String, String> out = new LinkedHashMap<>();
        for (final Xnav kid
            : formation.elements(Filter.withName("o")).collect(Collectors.toList())) {
            if ("∅".equals(kid.attribute("base").text().orElse(""))) {
                final String name = kid.attribute("name").text().get();
                if (!"ρ".equals(name)) {
                    out.put(name, ((Map<?, ?>) formas).get(name).toString());
                }
            }
        }
        return out;
    }

    private static Map<String, Xnav> helpers(final Xnav formation) {
        final Map<String, Xnav> out = new LinkedHashMap<>();
        for (final Xnav kid
            : formation.elements(Filter.withName("o")).collect(Collectors.toList())) {
            final String name = kid.attribute("name").text().orElse("");
            if (!name.isEmpty() && !"φ".equals(name)
                && !"∅".equals(kid.attribute("base").text().orElse(""))) {
                out.put(name, kid);
            }
        }
        return out;
    }

    private static String printed(final Protocol protocol, final int voids) {
        final String out;
        if (protocol.repeats()) {
            final StringBuilder head = new StringBuilder("loop");
            for (int idx = 0; idx < voids; ++idx) {
                head.append(" v").append(idx);
            }
            out = head.append(System.lineSeparator())
                .append(LoweringTest.rendered(protocol, "  ")).toString();
        } else {
            out = LoweringTest.rendered(protocol, "");
        }
        return out;
    }

    private static String rendered(final Protocol protocol, final String pad) {
        final StringBuilder out = new StringBuilder(64);
        for (final Step step : protocol.moves()) {
            out.append(pad).append(step.label()).append(" ← ");
            if (step.branches().isEmpty()) {
                out.append(step.atom());
                for (final String key : step.keys()) {
                    out.append(' ').append(key);
                }
                out.append(System.lineSeparator());
            } else {
                out.append(LoweringTest.forked(step, pad));
            }
        }
        return out.append(LoweringTest.ended(protocol, pad)).toString();
    }

    private static String forked(final Step step, final String pad) {
        final StringBuilder out = new StringBuilder(64);
        out.append("fork ").append(step.keys().get(0)).append(' ')
            .append(step.forma()).append(System.lineSeparator());
        final String[] arms = {"then", "else"};
        for (int idx = 0; idx < arms.length; ++idx) {
            final String arm = LoweringTest.rendered(
                step.branches().get(idx), String.format("%s    ", pad)
            );
            out.append(pad).append("  ").append(arms[idx])
                .append(System.lineSeparator()).append(arm);
        }
        return out.toString();
    }

    private static String ended(final Protocol protocol, final String pad) {
        final StringBuilder out = new StringBuilder(pad);
        if (protocol.again().isEmpty()) {
            out.append("answer ").append(protocol.answer())
                .append(' ').append(protocol.carrier());
        } else {
            out.append("repeat");
            for (final String key : protocol.again()) {
                out.append(' ').append(key);
            }
        }
        return out.append(System.lineSeparator()).toString();
    }
}
