/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Map;

/**
 * One label of {@code answers.xml}, as a page says it to a reader.
 *
 * <p>The engine labels what a copy gives: a locator such as
 * {@code Φ.number}, {@code D} for a datum, {@code T} for an error, a mark
 * that starts with {@code ?} when it cannot name anything, and one that
 * starts with {@code !} when a read fails. A page lists them under a mark,
 * and the reader is given words in place of the marks and the bare name in
 * place of a read that fails, so each label comes with a kind, which says
 * how a page draws it, and the words it is drawn with.</p>
 *
 * @since 0.76.0
 */
final class Label {

    /**
     * The words of the labels that have words of their own.
     */
    private static final Map<String, String> WORDS = Map.of(
        "D", "a datum",
        "T", "an error (T)",
        "?open-void", "an open void, filled by nobody",
        "?open-void-atom", "an open void, filled by an atom",
        "?can't-tell", "can't tell",
        "?cut", "cut",
        "!only-java-fills-ρ",
        "an error: this ^ is empty, and only Java fills it once an atom reads the object"
    );

    /**
     * The label, as the engine wrote it.
     */
    private final String text;

    /**
     * Ctor.
     *
     * @param label The label, as the engine wrote it
     */
    Label(final String label) {
        this.text = label;
    }

    /**
     * How a page draws it.
     *
     * @return One of {@code words}, {@code fails}, {@code skipped} and
     *  {@code name}
     */
    String kind() {
        final String found;
        if (Label.WORDS.containsKey(this.text)) {
            found = "words";
        } else if (this.text.startsWith("!")) {
            found = "fails";
        } else if (this.text.startsWith("?skipped")) {
            found = "skipped";
        } else if (this.text.startsWith("?")) {
            found = "words";
        } else {
            found = "name";
        }
        return found;
    }

    /**
     * What a page says.
     *
     * @return The words, the name a read fails on, or the label itself
     */
    String words() {
        final String found;
        if (Label.WORDS.containsKey(this.text)) {
            found = Label.WORDS.get(this.text);
        } else if (this.text.startsWith("!")) {
            found = this.text.substring(1);
        } else if (this.text.startsWith("?skipped")) {
            found = "skipped";
        } else if (this.text.startsWith("?")) {
            found = this.text.substring(1).replace('-', ' ');
        } else {
            found = this.text;
        }
        return found;
    }
}
