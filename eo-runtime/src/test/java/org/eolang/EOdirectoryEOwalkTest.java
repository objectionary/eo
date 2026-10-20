/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@code directory.walk} with a glob that is not a pattern.
 *
 * <p>The {@code -->} test in {@code directory.eo} only sees that the walk
 * fails, while the message it fails with is visible from Java alone.</p>
 *
 * @since 0.78.0
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
@ExtendWith(MktmpResolver.class)
final class EOdirectoryEOwalkTest {

    @Test
    void refusesAMalformedGlobNamingIt(@Mktmp final Path temp) throws IOException {
        Files.write(temp.resolve("a.txt"), new byte[0]);
        final Phi file = Phi.Φ.take("file").copy();
        file.put(0, new Data.ToPhi(temp.toString()));
        final Phi directory = Phi.Φ.take("directory").copy();
        directory.put(0, file);
        final Phi walk = directory.take("walk").copy();
        walk.put(0, new Data.ToPhi("[z-a]"));
        MatcherAssert.assertThat(
            "a glob that does not compile must be named in the message, but it was not",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(walk.take("length")).take(),
                "a walk with a malformed glob was expected to be refused"
            ).getMessage(),
            Matchers.containsString("Can't parse '[z-a]' as a glob pattern")
        );
    }
}
