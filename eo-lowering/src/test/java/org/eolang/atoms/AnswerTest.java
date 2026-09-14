/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.atoms;

import java.io.StringReader;
import javax.json.Json;
import javax.json.JsonReader;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Answer}.
 *
 * @since 0.77.0
 */
final class AnswerTest {

    @Test
    void readsDataOffTheDeltaFact() {
        MatcherAssert.assertThat(
            "the bytes must be read off Δ, not off the node, but they werent",
            AnswerTest.answer("{\"id\":1000001,\"𝑛\":\"⟦ Δ ⤍ 2A- ⟧\",\"Δ\":\"2A-\"}").data(),
            Matchers.equalTo("2A-")
        );
    }

    @Test
    void readsSymbolOffTheLambdaFact() {
        MatcherAssert.assertThat(
            "the λ name must be read off its fact, but it wasnt",
            AnswerTest.answer("{\"id\":1000001,\"𝑛\":\"⟦ λ ⤍ S7 ⟧\",\"λ\":\"S7\"}").lambda(),
            Matchers.equalTo("S7")
        );
    }

    @Test
    void knowsVoidAttribute() {
        MatcherAssert.assertThat(
            "an answer of ∅ must count as a void, but it didnt",
            AnswerTest.answer("{\"id\":1000001,\"∅\":true}").vacant(),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotTakeDataForVoid() {
        MatcherAssert.assertThat(
            "an answer carrying a node cannot count as a void, but it did",
            AnswerTest.answer("{\"id\":1000001,\"𝑛\":\"⟦ Δ ⤍ 2A- ⟧\",\"Δ\":\"2A-\"}").vacant(),
            Matchers.is(false)
        );
    }

    @Test
    void readsHeadOffItsFact() {
        MatcherAssert.assertThat(
            "the global object an application applies must be read off Φ, but it wasnt",
            AnswerTest.answer(
                "{\"id\":1000001,\"𝑛\":\"Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 01- ⟧ ) )\",\"Φ.\":\"number\"}"
            ).head(),
            Matchers.equalTo("number")
        );
    }

    @Test
    void hasNoHeadWithoutTheFact() {
        MatcherAssert.assertThat(
            "an answer of no Φ fact has no head, whatever its node spells, but it had one",
            AnswerTest.answer(
                "{\"id\":1000001,\"𝑛\":\"Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 01- ⟧ ) )\"}"
            ).head(),
            Matchers.is(Matchers.emptyString())
        );
    }

    @Test
    void spellsNodeForAMessage() {
        MatcherAssert.assertThat(
            "the node must be spelled as phino sent it, but it wasnt",
            AnswerTest.answer("{\"id\":1000001,\"𝑛\":\"⟦ q ↦ ∅ ⟧\"}").node(),
            Matchers.equalTo("⟦ q ↦ ∅ ⟧")
        );
    }

    private static Answer answer(final String json) {
        try (JsonReader reader = Json.createReader(new StringReader(json))) {
            return new Answer(reader.readObject());
        }
    }
}
