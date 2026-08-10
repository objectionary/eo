/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The XMIR of a program, as a clue reads it.
 *
 * <p>A clue is given a directory rather than a file, because an object in
 * one file is used by objects in others and no single file can be typed
 * on its own. The files come in the same order every time, so that the
 * tables come out the same every time too.</p>
 *
 * @since 0.67.0
 */
final class Xmirs {

    /**
     * The directory with the prepared XMIR files.
     */
    private final Path dir;

    /**
     * Ctor.
     * @param prepared The directory with the prepared XMIR files
     */
    Xmirs(final Path prepared) {
        this.dir = prepared;
    }

    /**
     * Every formation of the program.
     *
     * <p>A formation is an object with no {@code @base}: it is not a copy
     * of anything, it is written down as it is. Two other kinds of objects
     * have no base either and are not formations: data, which carries its
     * bytes as text, and the {@code λ} marker of an atom, which names a
     * body implemented in Java.</p>
     *
     * @return The formations, file by file, in the order they appear in
     *  the code
     * @throws IOException If a file cannot be read
     */
    Collection<XML> formations() throws IOException {
        return this.matching(
            "//o[not(@base) and not(@name='λ') and not(text()[normalize-space()])]"
        );
    }

    /**
     * Every dispatch of the program.
     *
     * <p>A dispatch is an object whose base begins with a dot: it takes an
     * attribute from the object below it. Every composite base was split
     * into one such object per step before any clue looked at the XMIR, so
     * a chain like {@code x.next.foo} is three objects here and not one.</p>
     *
     * @return The dispatches, file by file, in the order they appear in
     *  the code
     * @throws IOException If a file cannot be read
     */
    Collection<XML> dispatches() throws IOException {
        return this.matching("//o[starts-with(@base, '.')]");
    }

    /**
     * Every reference of the program.
     *
     * <p>A reference names an object instead of taking an attribute from
     * one: {@code ξ.t} names something bound nearby, {@code Φ.number}
     * names what the whole program knows. Both carry exactly one name,
     * since a longer path was split into dispatches before any clue looked
     * at the XMIR.</p>
     *
     * @return The references, file by file, in the order they appear in
     *  the code
     * @throws IOException If a file cannot be read
     */
    Collection<XML> references() throws IOException {
        return this.matching("//o[starts-with(@base, 'ξ.') or starts-with(@base, 'Φ.')]");
    }

    /**
     * The locator of every object of the program.
     * @return The locators
     * @throws IOException If a file cannot be read
     */
    Collection<String> locators() throws IOException {
        final Collection<String> found = new ArrayList<>(0);
        for (final XML xmir : this.documents()) {
            found.addAll(xmir.xpath("//o/@loc"));
        }
        return found;
    }

    /**
     * Every application of the program.
     *
     * <p>An application is an object with something bound into a copy of
     * what it names: its arguments are the children carrying {@code as}. A
     * dispatch can be one too — {@code x.plus 5} applies the {@code 5} to
     * whatever {@code x.plus} turns out to be.</p>
     *
     * @return The applications, file by file, in the order they appear in
     *  the code
     * @throws IOException If a file cannot be read
     */
    Collection<XML> applications() throws IOException {
        return this.matching("//o[o/@as]");
    }

    /**
     * Every object of the program the given XPath matches.
     * @param xpath The XPath
     * @return The objects, file by file
     * @throws IOException If a file cannot be read
     */
    private Collection<XML> matching(final String xpath) throws IOException {
        final Collection<XML> found = new ArrayList<>(0);
        for (final XML xmir : this.documents()) {
            found.addAll(xmir.nodes(xpath));
        }
        return found;
    }

    /**
     * Every XMIR document of the program.
     * @return The documents
     * @throws IOException If the directory cannot be walked
     */
    private Collection<XML> documents() throws IOException {
        final Collection<XML> read = new ArrayList<>(0);
        for (final Path source : this.sources()) {
            read.add(new XMLDocument(source));
        }
        return read;
    }

    /**
     * Every XMIR file of the program, in the same order every time.
     * @return The files
     * @throws IOException If the directory cannot be walked
     */
    private Collection<Path> sources() throws IOException {
        try (Stream<Path> found = Files.walk(this.dir)) {
            return found
                .filter(path -> path.toString().endsWith(".xmir"))
                .sorted()
                .collect(Collectors.toList());
        }
    }
}
