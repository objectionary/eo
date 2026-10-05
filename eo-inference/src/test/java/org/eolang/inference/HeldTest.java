/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XMLDocument;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Held}.
 *
 * @since 0.69.0
 */
final class HeldTest {

    @Test
    void dropsTheMarkOfATerminationFromWhatAVoidHolds() {
        MatcherAssert.assertThat(
            "a value that may terminate must be read as the type it otherwise is, but it wasnt",
            new Held(
                new XMLDocument(
                    String.join(
                        "",
                        "<provides><type id='Φ.reader'>",
                        "<attr name='code' type='Φ.reader.code' void='true' holds='Φ.number?'/>",
                        "</type></provides>"
                    )
                )
            ).all(),
            Matchers.hasEntry("Φ.reader.code", "Φ.number")
        );
    }
}
