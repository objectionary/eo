/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicInteger;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.xembly.Directives;
import org.xembly.ImpossibleModificationException;
import org.xembly.Xembler;

/**
 * Tests for {@link FpJavaInput}.
 *
 * @since 0.56.7
 */
@ExtendWith(MktmpResolver.class)
final class FpJavaInputTest {

    @Test
    void placesTheJavaAsInputFiles(@Mktmp final Path temp) throws ImpossibleModificationException, IOException {
        final Path source = temp.resolve("foo.eo");
        Files.write(source, "# Foo.\n[] > foo".getBytes(StandardCharsets.UTF_8));
        final Path java = temp.resolve("Foo.java");
        final String content = "public final class Foo {}";
        new FpJavaInput(
            new AtomicInteger(),
            new Xnav(
                new Xembler(new Directives().add("java").set(content)).xml()
            ),
            new Place("foo").make(temp, MjAssemble.EO),
            java
        ).apply(source, java);
        MatcherAssert.assertThat(
            "Placed Java code does not match with expected",
            Files.readString(java),
            Matchers.equalTo(content)
        );
    }
}
