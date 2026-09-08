/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Call}.
 * @since 0.76.0
 */
final class CallTest {

    @Test
    void wrapsNumberAndDataizesNumber() {
        MatcherAssert.assertThat(
            "a number receiver must be the object the void holds, and the answer dataized, but it isnt",
            CallTest.call(
                new Dispatch(
                    "s1", "minus", Arrays.asList("sym:v0", "number:3F-F0-00-00-00-00-00-00"),
                    "number"
                ),
                Collections.singletonMap("x", "number")
            ),
            Matchers.equalTo(
                String.join(
                    "",
                    "new Dataized(new PhApplication(new PhDispatch(this.take(\"x\"), ",
                    "\"minus\"), new Bind(0, new Data.ToPhi(",
                    "Double.longBitsToDouble(0x3FF0000000000000L))))).asNumber()"
                )
            )
        );
    }

    @Test
    void takesMethodWithoutArguments() {
        MatcherAssert.assertThat(
            "a method without arguments must be taken and dataized as a bool, but it isnt",
            CallTest.call(
                new Dispatch("s1", "is-nan", Collections.singletonList("sym:v0"), "bool"),
                Collections.singletonMap("x", "number")
            ),
            Matchers.equalTo(
                "new Dataized(new PhDispatch(this.take(\"x\"), \"is-nan\")).asBool()"
            )
        );
    }

    @Test
    void readsStringReceiverAsText() {
        MatcherAssert.assertThat(
            "a string receiver must be the object the void holds, and bytes taken, but it isnt",
            CallTest.call(
                new Dispatch("s1", "trimmed", Collections.singletonList("sym:v0"), "string"),
                Collections.singletonMap("t", "string")
            ),
            Matchers.equalTo(
                String.join(
                    "",
                    "new Dataized(new PhDispatch(",
                    "this.take(\"t\"), \"trimmed\")).take()"
                )
            )
        );
    }

    @Test
    void holdsObjectAnswerAsItIs() {
        MatcherAssert.assertThat(
            "an object answer must stay the Phi the call makes, but it was dataized",
            CallTest.call(
                new Dispatch(
                    "s1", "with", Arrays.asList("sym:v0", "bool:FF-"), "object"
                ),
                Collections.singletonMap("items", "tuple")
            ),
            Matchers.equalTo(
                String.join(
                    "",
                    "new PhApplication(new PhDispatch(this.take(\"items\"), \"with\"), ",
                    "new Bind(0, new Data.ToPhi(true)))"
                )
            )
        );
    }

    @Test
    void appliesSeveralArgumentsByPosition() {
        final Map<String, String> voids = new LinkedHashMap<>();
        voids.put("t", "string");
        voids.put("a", "number");
        voids.put("b", "number");
        MatcherAssert.assertThat(
            "every argument must be applied at its own position, but it isnt",
            CallTest.call(
                new Dispatch("s1", "slice", Arrays.asList("sym:v0", "sym:v1", "sym:v2"), "object"),
                voids
            ),
            Matchers.endsWith(
                ", new Bind(0, this.take(\"a\")), new Bind(1, this.take(\"b\")))"
            )
        );
    }

    private static String call(final Step step, final Map<String, String> voids) {
        return new Call(
            step,
            new Rendering(
                new Protocol(Collections.singletonList(step), "sym:s1", step.forma()),
                voids
            )
        ).text();
    }
}
