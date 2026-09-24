/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link Ref}.
 *
 * @since 0.75.0
 */
final class RefTest {

    @Test
    void marksWhatWasReachedThroughAVoid() {
        MatcherAssert.assertThat(
            "a copy reached through a void must say so, and so must a bind relayed there",
            new Xembler(
                new Directives().add("type").append(
                    new Ref(
                        "Φ.loud",
                        Map.of("Φ.loud.message", "Φ.call.α0", "Φ.loud.size", "Φ.call.α1"),
                        Collections.emptyList(),
                        true,
                        List.of("Φ.loud.message")
                    ).directives()
                )
            ).xmlQuietly(),
            XhtmlMatchers.hasXPaths(
                "/type/ref[@loc='Φ.loud' and @witnessed='true']",
                "/type/ref/bind[@void='Φ.loud.message' and @witnessed='true']",
                "/type/ref/bind[@void='Φ.loud.size' and not(@witnessed)]"
            )
        );
    }

    @Test
    void saysNothingOfAPlainCopy() {
        MatcherAssert.assertThat(
            "a copy the program writes in so many words must not be marked, but it was",
            new Xembler(
                new Directives().add("type").append(
                    new Ref("Φ.box", Map.of("Φ.box.item", "Φ.held.α0")).directives()
                )
            ).xmlQuietly(),
            XhtmlMatchers.hasXPaths(
                "/type/ref[@loc='Φ.box' and not(@witnessed)]",
                "/type/ref/bind[@void='Φ.box.item' and not(@witnessed)]"
            )
        );
    }
}
