/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Attr}.
 *
 * @since 0.78.0
 */
final class AttrTest {

    @Test
    void hidesTheVoidItDeclaresFromAReadByThePlainName() {
        MatcherAssert.assertThat(
            "a void declared by its plain name must not be found by that name, but it was",
            new PhApplication(
                new PhDefault(new Attrs(new Attr("tail"))), 0, new Data.ToPhi(3L)
            ).take("tail"),
            Matchers.instanceOf(PhTerminator.class)
        );
    }
}
