/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.Map;
import java.util.stream.Collectors;
import org.eolang.jucs.ClasspathSource;
import org.eolang.lowering.Digest;
import org.eolang.lowering.Phino;
import org.eolang.printer.Xmir;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.Xtory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link Lowering}.
 *
 * <p>Each YAML pack tells the whole story of one program: the EO source
 * it starts from, the EO it prints as after the lowering step, and the
 * Java body of the atom the step wrote, when it wrote one. The packs are
 * the visual record of what lowering does, so a change in what phino
 * folds, how the markers come back, or how the table renders shows up
 * here as a readable diff. They hold only when a phino binary of the
 * pinned version is installed, which is what CI arranges.</p>
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
                LoweringTest.maven(this.temp, story).execute(new PpLower())
                    .targetPath().resolve(Lowering.DIR).resolve("atoms")
                    .resolve(String.format("%s.java", new Digest(body).hex()))
            ),
            Matchers.equalTo(body)
        );
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
}
