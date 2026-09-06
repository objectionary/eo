/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Rendering}.
 * @since 0.76.0
 */
final class RenderingTest {

    @Test
    void spellsNumberLiteralAsBits() {
        MatcherAssert.assertThat(
            "a number literal must spell as the double of its bits, but it doesnt",
            new Rendering(
                new Protocol(Collections.emptyList(), "sym:v0", "number"),
                Collections.singletonMap("x", "number")
            ).expression("number:40-00-00-00-00-00-00-00"),
            Matchers.equalTo("Double.longBitsToDouble(0x4000000000000000L)")
        );
    }

    @Test
    void carriesStringAsByteArray() {
        MatcherAssert.assertThat(
            "a string must be carried as bytes in Java, but it isnt",
            new Rendering(
                new Protocol(Collections.emptyList(), "sym:v0", "string"),
                Collections.singletonMap("t", "string")
            ).type("sym:v0"),
            Matchers.equalTo("byte[]")
        );
    }

    @Test
    void findsFormaOfStepInsideArm() {
        MatcherAssert.assertThat(
            "the forma of a step nested in an arm must be found, but it wasnt",
            new Rendering(
                new Protocol(
                    Collections.singletonList(
                        new Fork(
                            "s1", "L_bool_if", "sym:v0",
                            new Protocol(
                                Collections.singletonList(
                                    new Application(
                                        "s2", "L_bytes_size", Collections.singletonList("sym:v1")
                                    )
                                ),
                                "sym:s2",
                                "number"
                            ),
                            new Protocol(Collections.emptyList(), "number:11-", "number")
                        )
                    ),
                    "sym:s1",
                    "number"
                ),
                Collections.singletonMap("f", "bool")
            ).forma("sym:s2"),
            Matchers.equalTo("number")
        );
    }

    @Test
    void readsBoolVoidWithoutFinal() {
        MatcherAssert.assertThat(
            "the read of a void must be spelled without final, so a loop may rebind it, but it isnt",
            new Rendering(
                new Protocol(Collections.emptyList(), "sym:v0", "bool"),
                Collections.singletonMap("f", "bool")
            ).reading(0),
            Matchers.equalTo("boolean v0 = new Dataized(this.take(\"f\")).asBool();")
        );
    }

    @Test
    void readsTupleVoidAsPhi() {
        MatcherAssert.assertThat(
            "a tuple void must be held as the Phi itself, but it isnt",
            new Rendering(
                new Protocol(Collections.emptyList(), "sym:v0", "tuple"),
                Collections.singletonMap("items", "tuple")
            ).reading(0),
            Matchers.equalTo("Phi v0 = this.take(\"items\");")
        );
    }

    @Test
    void typesObjectAsPhi() {
        MatcherAssert.assertThat(
            "an object a tuple answers must be typed as Phi, but it isnt",
            new Rendering(
                new Protocol(
                    Collections.singletonList(
                        new Application(
                            "s1", "L_tuple_at",
                            Arrays.asList("sym:v0", "number:3F-F0-00-00-00-00-00-00")
                        )
                    ),
                    "sym:s1", "object"
                ),
                Collections.singletonMap("items", "tuple")
            ).type("sym:s1"),
            Matchers.equalTo("Phi")
        );
    }

    @Test
    void dispatchesIntoTheTupleForItsElement() {
        MatcherAssert.assertThat(
            "the element of a tuple must be taken through the at attribute, but it isnt",
            new Rendering(
                new Protocol(
                    Collections.singletonList(
                        new Application(
                            "s1", "L_tuple_at",
                            Arrays.asList("sym:v0", "number:3F-F0-00-00-00-00-00-00")
                        )
                    ),
                    "sym:s1", "object"
                ),
                Collections.singletonMap("items", "tuple")
            ).applied(
                new Application(
                    "s1", "L_tuple_at", Arrays.asList("sym:v0", "number:3F-F0-00-00-00-00-00-00")
                )
            ),
            Matchers.equalTo(
                "new PhWith(new PhCopy(new PhMethod(v0, \"at\")), 0, new Data.ToPhi(Double.longBitsToDouble(0x3FF0000000000000L)))"
            )
        );
    }

    @Test
    void refusesOperandOfForeignForma() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Rendering(
                new Protocol(Collections.emptyList(), "sym:v0", "bytes"),
                Collections.singletonMap("b", "bytes")
            ).applied(
                new Application(
                    "s1", "L_number_plus",
                    Arrays.asList("sym:v0", "number:3F-F0-00-00-00-00-00-00")
                )
            ),
            "a number operation over a bytes operand cannot spell, but it did"
        );
    }
}
