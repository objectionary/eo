/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * What every object of a program is taken off.
 *
 * <p>A name written after a dot says in the text what it comes off, and the
 * parser writes that down beside the name, which is what {@link Xmirs} reads. A
 * name written by itself says the same thing silently: {@code grove} inside the
 * body of {@code app} is the {@code grove} of that {@code app}, and nothing
 * stands beside it to say so. Both are gathered here into one answer, since a
 * formation that declares a receiver has it filled either way, and a body that
 * reads through {@code ^} knows nothing until it is (#8278).</p>
 *
 * <p>What a name written by itself comes off is the object that binds it, which
 * is the owner of the locator the name points at: {@code ξ.grove} points at
 * {@code Φ.app.grove}, so it is taken off the {@code Φ.app}. That is why the
 * table is read here instead of the walk outwards being made again —
 * {@link Scope} made it once for {@link Links}, and a second walk is a second
 * chance to disagree with the first.</p>
 *
 * <p>A fully-qualified name is taken off nothing, being what the whole program
 * knows however deep it is written. Neither is {@code ^} taken off anything: it
 * names the receiver rather than asking for something of it.</p>
 *
 * @since 0.71.0
 */
final class Taken {

    /**
     * The XMIR of the program.
     */
    private final Xmirs world;

    /**
     * The links table, where every name has been resolved already.
     */
    private final Pairs written;

    /**
     * Ctor.
     * @param xmirs The XMIR of the program
     * @param links The links table, as the rules wrote it, which says where
     *  every name of the program points
     */
    Taken(final Xmirs xmirs, final Pairs links) {
        this.world = xmirs;
        this.written = links;
    }

    /**
     * What every object of the program takes its attribute from.
     * @return The locator of the object taken from, by the locator of the
     *  object doing the taking, without the ones that take from nothing
     * @throws IOException If a file cannot be read
     */
    Map<String, String> all() throws IOException {
        final Map<String, String> found = new HashMap<>(this.world.receivers());
        final Map<String, String> pairs = this.written.all();
        for (final XML reference : this.world.references()) {
            final Noted named = new Noted(reference);
            final String owner = Taken.owner(named, pairs);
            if (!owner.isEmpty()) {
                found.put(named.says("loc"), owner);
            }
        }
        return found;
    }

    private static String owner(final Noted named, final Map<String, String> pairs) {
        final String base = named.says("base");
        String found = "";
        if (base.startsWith("ξ.") && !"ξ.ρ".equals(base)) {
            final String target = pairs.getOrDefault(named.says("loc"), "");
            if (target.contains(".")) {
                found = target.substring(0, target.lastIndexOf('.'));
            }
        }
        return found;
    }
}
