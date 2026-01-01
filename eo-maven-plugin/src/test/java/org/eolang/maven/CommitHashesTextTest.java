/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Together;
import com.yegor256.WeAreOnline;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link CommitHashesText}.
 *
 * @since 0.37.0
 */
@ExtendWith(WeAreOnline.class)
final class CommitHashesTextTest {

    @Test
    void downloadsDefaultList() throws Exception {
        MatcherAssert.assertThat(
            "CommitHashesText downloads the default list of hashes from Objectionary",
            new CommitHashesText().asString(),
            Matchers.containsString("master")
        );
    }

    @Test
    void isThreadSafe() {
        final CommitHashesText text = new CommitHashesText();
        MatcherAssert.assertThat(
            "Can be used in different threads without NPE",
            new Together<>(
                thread -> text.asString() != null
            ),
            Matchers.not(Matchers.hasItems(false))
        );
    }
}
