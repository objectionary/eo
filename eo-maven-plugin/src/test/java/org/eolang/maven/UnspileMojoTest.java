/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link UnspileMojo}.
 *
 * @since 0.1
 */
@ExtendWith(MktmpResolver.class)
final class UnspileMojoTest {

    @Test
    void cleans(@Mktmp final Path temp) throws IOException {
        final Path generated = Paths.get("generated");
        final Path classes = Paths.get("classes");
        final Path foo = Paths.get("a/b/c/foo.class");
        final FakeMaven maven = new FakeMaven(temp);
        new HmBase(temp).save("abc", foo);
        new HmBase(temp).save("xxx", generated.resolve("a/b/c/foo.java"));
        new HmBase(temp).save("cde", classes.resolve("foo.txt"));
        maven.with("generatedDir", generated.toFile())
            .with("classesDir", classes.toFile())
            .execute(UnspileMojo.class);
        MatcherAssert.assertThat(
            String.format(
                "UnspileMojo unable to remove %s class",
                foo
            ),
            Files.exists(foo),
            Matchers.is(false)
        );
    }
}
