/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicInteger;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Tests for {@link FpJavaGenerated}.
 *
 * @since 0.56.7
 */
@ExtendWith(MktmpResolver.class)
final class FpJavaGeneratedTest {

    @Test
    void placesTheJavaAsInputFiles(@Mktmp final Path temp) throws Exception {
        final Path source = temp.resolve("foo.eo");
        Files.write(source, "# Foo.\n[] > foo".getBytes(StandardCharsets.UTF_8));
        final Path java = temp.resolve("Foo.java");
        final String content = "public final class Foo {}";
        new FpJavaGenerated(
            new Xnav(new Xembler(new Directives().add("java").set(content)).xml()),
            new FileGenerationReport(
                new AtomicInteger(), new Place("foo").make(temp, MjAssemble.EO), java
            )
        ).apply(source, java);
        MatcherAssert.assertThat(
            "Placed Java code does not match with expected",
            new TextOf(java).asString(),
            Matchers.equalTo(content)
        );
    }
}
