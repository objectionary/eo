/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;
import java.io.StringWriter;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Channel}.
 *
 * @since 0.77.0
 */
final class ChannelTest {

    @Test
    void writesAnswerAsOneJsonLine() throws IOException {
        final StringWriter out = new StringWriter();
        new Channel(out).answer(17, "⟦ Δ ⤍ 2A- ⟧");
        MatcherAssert.assertThat(
            "the answer must carry the fire id and the node under 𝑛, but it didnt",
            out.toString(),
            Matchers.equalTo("{\"id\":17,\"𝑛\":\"⟦ Δ ⤍ 2A- ⟧\"}\n")
        );
    }

    @Test
    void asksAboutAttributeOfFire() throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(new Oracle(channel, "⟦ Δ ⤍ 01- ⟧")).start();
        MatcherAssert.assertThat(
            "the question must be answered by the routed node, but it wasnt",
            channel.ask(5, "ρ", false),
            Matchers.equalTo("⟦ Δ ⤍ 01- ⟧")
        );
    }

    @Test
    void spellsQuestionWithFreshId() throws Exception {
        final StringWriter out = new StringWriter();
        final Channel channel = new Channel(out);
        new Thread(new Oracle(channel, "")).start();
        channel.ask(9, "x", true);
        MatcherAssert.assertThat(
            "the question must name the fire, the attribute and the mode, but it doesnt",
            out.toString(),
            Matchers.equalTo("{\"id\":1000001,\"of\":9,\"attr\":\"x\",\"reduce\":true}\n")
        );
    }

    @Test
    void refusesAnswerToUnknownQuestion() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Channel(new StringWriter()).answered(42, "⟦ ⟧"),
            "an answer nobody asked for must be refused, but it wasnt"
        );
    }
}
