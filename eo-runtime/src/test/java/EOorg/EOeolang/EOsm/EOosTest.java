/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsm; // NOPMD

import org.eolang.Dataized;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOos}.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOosTest {

    @Test
    void readsSystemFamily() {
        MatcherAssert.assertThat(
            "Dataization of uname object should not return null",
            new Dataized(
                new EOos$EOname()
            ).take(String.class),
            Matchers.is(Matchers.notNullValue())
        );
    }

    @Test
    void readsSystemFamilyCorrectly() {
        MatcherAssert.assertThat(
            "Object uname returns incorrect system name",
            new Dataized(
                new EOos$EOname()
            ).take(String.class),
            Matchers.equalTo(System.getProperty("os.name"))
        );
    }
}
