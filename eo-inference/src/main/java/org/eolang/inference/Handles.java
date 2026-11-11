/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * The names an author gave the objects the parser hides.
 *
 * <p>An object written with {@code >> stream} is named by the parser after
 * the line and column it is written at, {@code a🌵147-4}, and so is every
 * locator under it: {@code Φ.file.open.a🌵147-4.read}. That name means
 * something to the engine and nothing to a reader, who wrote {@code stream}
 * and looks for it. The parser keeps the name the author gave beside the one
 * it made, and this puts it back wherever a page tells a name: each segment
 * the parser made is the name the author gave, with the made one as its
 * hover text, and the whole name keeps its locator as hover text of its
 * own. An object its author gave no name, a bare {@code >>}, keeps the
 * name the parser made.</p>
 *
 * <p>Only the words change. What a mark tells, its colour and its counts
 * stay what the table says.</p>
 *
 * @since 0.76.0
 */
final class Handles {

    /**
     * A name as a page tells it, a locator with no spaces, brackets or
     * quotes in it.
     */
    private static final Pattern NAME = Pattern.compile("Φ(?:\\.[^\\s.<>&\"',()\\[\\]{}]++)++");

    /**
     * The start of a segment the parser made for a {@code >>}.
     */
    private static final String MADE = "a🌵";

    /**
     * The names the author gave, by the locator of their object.
     */
    private final Map<String, String> names;

    /**
     * Ctor.
     *
     * @param given The names the author gave, by the locator of their
     *  object
     */
    Handles(final Map<String, String> given) {
        this.names = given;
    }

    /**
     * Spell every name a page tells with the names the author gave.
     *
     * @param page The page, with a {@code said} for every answer it tells
     */
    void spelled(final Element page) {
        final List<Node> texts = new ArrayList<>(0);
        final NodeList said = page.getElementsByTagName("said");
        for (int entry = 0; entry < said.getLength(); entry = entry + 1) {
            final NodeList kids = said.item(entry).getChildNodes();
            for (int kid = 0; kid < kids.getLength(); kid = kid + 1) {
                if (kids.item(kid).getNodeType() == Node.TEXT_NODE) {
                    texts.add(kids.item(kid));
                }
            }
        }
        texts.forEach(this::respelled);
    }

    private void respelled(final Node text) {
        final String words = text.getNodeValue();
        final Matcher matcher = Handles.NAME.matcher(words);
        final DocumentFragment pieces = text.getOwnerDocument().createDocumentFragment();
        int cursor = 0;
        while (matcher.find()) {
            if (this.locators(matcher.group()).anyMatch(this::handled)) {
                Handles.appended(pieces, words.substring(cursor, matcher.start()));
                pieces.appendChild(this.name(text.getOwnerDocument(), matcher.group()));
                cursor = matcher.end();
            }
        }
        if (pieces.hasChildNodes()) {
            Handles.appended(pieces, words.substring(cursor));
            text.getParentNode().replaceChild(pieces, text);
        }
    }

    private Element name(final Document page, final String full) {
        final Element name = page.createElement("name");
        name.setAttribute("loc", full);
        final StringBuilder plain = new StringBuilder(full.length());
        this.locators(full).forEach(
            loc -> {
                if (this.handled(loc)) {
                    Handles.appended(name, plain.toString());
                    plain.setLength(0);
                    name.appendChild(this.handle(page, loc));
                } else {
                    plain.append(Handles.segment(loc));
                }
                if (loc.length() < full.length()) {
                    plain.append('.');
                }
            }
        );
        Handles.appended(name, plain.toString());
        return name;
    }

    private Element handle(final Document page, final String loc) {
        final Element handle = page.createElement("handle");
        handle.setAttribute("hidden", Handles.segment(loc));
        handle.appendChild(page.createTextNode(this.names.get(loc)));
        return handle;
    }

    private boolean handled(final String loc) {
        return Handles.segment(loc).startsWith(Handles.MADE) && this.names.containsKey(loc);
    }

    private Stream<String> locators(final String full) {
        return IntStream.rangeClosed(1, full.length())
            .filter(end -> end == full.length() || full.charAt(end) == '.')
            .mapToObj(end -> full.substring(0, end));
    }

    private static String segment(final String loc) {
        return loc.substring(loc.lastIndexOf('.') + 1);
    }

    private static void appended(final Node parent, final String words) {
        if (!words.isEmpty()) {
            parent.appendChild(parent.getOwnerDocument().createTextNode(words));
        }
    }
}
