/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
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
 * Tests for {@link PlacedJava}.
 *
 * @since 0.56.7
 */
@ExtendWith(MktmpResolver.class)
final class PlacedJavaTest {

    @Test
    void placesJavaGeneratedCode(@Mktmp final Path temp) throws Exception {
        final Path target = temp.resolve("target").resolve("Foo.java");
        final String expected = "public final class Main {}";
        final Path generated = temp.resolve("generated-sources");
        final Xnav java = new Xnav(
            new Xembler(
                new Directives()
                    .add("java")
                    .set(expected)
            ).xml()
        );
        new PlacedJava(
            new FpJavaGenerated(
                new AtomicInteger(),
                java,
                generated,
                target
            ),
            target,
            generated
        ).exec(java);
        MatcherAssert.assertThat(
            "Generated Java code does not match with expected",
            new TextOf(target.toFile()).asString(),
            Matchers.equalTo(expected)
        );
    }
}
