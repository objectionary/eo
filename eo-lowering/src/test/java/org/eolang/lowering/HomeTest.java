/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Home}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class HomeTest {

    @Test
    void listsBoxedVariantsOfOtherDocuments(@Mktmp final Path temp) throws IOException {
        final Home home = new Home(temp);
        for (final String name : new String[] {"b", "a", "c"}) {
            Files.createDirectories(home.boxed(name).getParent());
            Files.write(home.boxed(name), new byte[0]);
        }
        MatcherAssert.assertThat(
            "the boxed variants of the other documents must be listed in order, but they arent",
            home.others("b"),
            Matchers.contains(home.boxed("a"), home.boxed("c"))
        );
    }

    @Test
    void makesFreshRunDirectories(@Mktmp final Path temp) throws IOException {
        final Home home = new Home(temp);
        MatcherAssert.assertThat(
            "two runs must not share a directory, but they do",
            home.run(),
            Matchers.not(Matchers.equalTo(home.run()))
        );
    }
}
