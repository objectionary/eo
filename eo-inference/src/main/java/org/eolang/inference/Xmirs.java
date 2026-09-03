/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
     * The documents, read on the first question and kept for the rest.
     *
     * <p>A clue asks several questions of the same corpus and there are
     * ten of them, so reading and parsing every file again for each one
     * costs ten passes where one is enough. The list holds either nothing,
     * before the first question, or the single collection of documents.</p>
     */
    private final List<Collection<XML>> read;

    /**
     * Ctor.
     * @param prepared The directory with the prepared XMIR files
     */
    Xmirs(final Path prepared) {
        this.dir = prepared;
        this.read = new ArrayList<>(1);
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
     * Every datum of the program.
     *
     * <p>A datum has no {@code @base}, like a formation, and carries its
     * bytes as text, unlike one. It is the ground the whole program stands
     * on and the one kind of object that is not a copy of anything: there is
     * nothing to know about {@code 01-} beyond that it is what it is.</p>
     *
     * @return The data, file by file, in the order they appear in the code
     * @throws IOException If a file cannot be read
     */
    Collection<XML> data() throws IOException {
        return this.matching("//o[@loc and not(@base) and text()[normalize-space()]]");
    }

    /**
     * Every termination of the program.
     *
     * <p>The {@code T} of the code, which the parser writes down as an
     * object based on the terminator sign. It names no other object, so nothing
     * looks for what it is a copy of: it is not a copy of anything, it is
     * the one answer that fits everywhere.</p>
     *
     * @return The terminations, file by file, in the order they appear in
     *  the code
     * @throws IOException If a file cannot be read
     */
    Collection<XML> terminators() throws IOException {
        return this.matching("//o[@loc and @base='⊥']");
    }

    /**
     * Every void of the program.
     *
     * <p>An object with nothing behind it, written as a base of its own in
     * the text. It names no other object, since what it holds is not decided
     * here but by whoever fills it.</p>
     *
     * @return The voids, file by file, in the order they appear in the code
     * @throws IOException If a file cannot be read
     */
    Collection<XML> voids() throws IOException {
        return this.matching("//o[@loc and @base='∅']");
    }

    /**
     * The object every file of the program is about.
     *
     * <p>A file carries one object at the top and the package it declares
     * goes into the locator of that object, so {@code minus} in the package
     * {@code number} is {@code Φ.number.minus}. That is how an attribute
     * comes to live in a file of its own.</p>
     *
     * @return The named top-level objects, one per file, in the order the
     *  files come in
     * @throws IOException If a file cannot be read
     */
    Collection<XML> roots() throws IOException {
        return this.matching("/object/o[@name]");
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
    Collection<Site> dispatches() throws IOException {
        final Collection<Site> found = new ArrayList<>(0);
        for (final XML dispatch : this.matching("//o[starts-with(@base, '.')]")) {
            found.add(new Site(new Xnav(dispatch.inner())));
        }
        return found;
    }

    /**
     * Every application of the program.
     *
     * <p>An application puts something into the voids of the object it
     * copies, and the place of an argument is what says which void it goes
     * into. That is the only place in the text where a void is answered, so
     * it is where a name taken from one stops being a question.</p>
     *
     * @return The applications, file by file, in the order they appear in
     *  the code
     * @throws IOException If a file cannot be read
     */
    Collection<XML> applications() throws IOException {
        return this.matching("//o[@loc][o[starts-with(@as, 'α')][@loc]]");
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
     * What every dispatch takes its attribute from.
     *
     * <p>The receiver of a dispatch is the child that carries no {@code @as},
     * the arguments being the ones that do, and the parser gives it the
     * locator of the dispatch with {@code ρ} on the end. It is looked for by
     * that locator rather than by the absence alone, because a formation
     * bound inside a dispatch carries no {@code @as} either.</p>
     *
     * @return The locator of the receiver, by the locator of the dispatch
     * @throws IOException If a file cannot be read
     */
    Map<String, String> receivers() throws IOException {
        final Map<String, String> found = new HashMap<>(0);
        for (final XML xmir : this.documents()) {
            for (final XML node : xmir.nodes("//o[@loc][o[@loc][not(@as)]]")) {
                final Xnav owner = new Xnav(node.inner());
                final String loc = new Noted(owner).says("loc");
                Xmirs.bare(owner)
                    .map(kid -> new Noted(kid).says("loc"))
                    .filter(kid -> kid.equals(loc.concat(".ρ")))
                    .findFirst()
                    .ifPresent(kid -> found.put(loc, kid));
            }
        }
        return found;
    }

    private static Stream<Xnav> bare(final Xnav owner) {
        return owner.elements(
            Filter.all(Filter.withName("o"), Filter.not(Filter.hasAttribute("as")))
        );
    }

    private Collection<XML> matching(final String xpath) throws IOException {
        final Collection<XML> found = new ArrayList<>(0);
        for (final XML xmir : this.documents()) {
            found.addAll(xmir.nodes(xpath));
        }
        return found;
    }

    private Collection<XML> documents() throws IOException {
        if (this.read.isEmpty()) {
            final Collection<XML> found = new ArrayList<>(0);
            for (final Path source : this.sources()) {
                found.add(new XMLDocument(source));
            }
            this.read.add(found);
        }
        return this.read.get(0);
    }

    private Collection<Path> sources() throws IOException {
        try (Stream<Path> found = Files.walk(this.dir)) {
            return found
                .filter(path -> path.toString().endsWith(".xmir"))
                .filter(Files::isRegularFile)
                .sorted()
                .collect(Collectors.toList());
        }
    }
}
