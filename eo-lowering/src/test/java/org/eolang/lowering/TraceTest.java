/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Trace}.
 * @since 0.76.0
 */
final class TraceTest {

    @Test
    void reportsPartialRun() {
        MatcherAssert.assertThat(
            "a run that parked something cannot be total, but it is",
            new Trace(false, Collections.emptyList()).total(),
            Matchers.is(false)
        );
    }

    @Test
    void listsRecords() {
        MatcherAssert.assertThat(
            "the records must come back as given, but they didnt",
            new Trace(
                true,
                Collections.singletonList(new Evaluation("Sym_v0\t⟦⟧"))
            ).records().get(0).name(),
            Matchers.equalTo("Sym_v0")
        );
    }
}
