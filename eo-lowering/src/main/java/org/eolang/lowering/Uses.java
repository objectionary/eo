/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.Map;
import java.util.TreeSet;
import org.cactoos.Scalar;
import org.cactoos.bytes.Sha256DigestOf;
import org.cactoos.io.InputOf;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Synced;
import org.cactoos.scalar.Unchecked;
import org.cactoos.text.HexOf;
import org.cactoos.text.UncheckedText;

/**
 * The part of the world that one entry uses, as a hash.
 *
 * <p>phino meets, while it runs an entry, only the objects that the entry
 * refers to, the objects that those objects refer to, and so on. An object
 * refers to another top object by its full locator, so this class follows
 * such references from the entry through the copies, one top object at a
 * time, until nothing new is found. The hash is made of the entry, the two
 * objects of the world that wrap every entry, the hash of every copy that
 * was reached, and every reference that no copy holds, since a copy that
 * holds it later must change the hash too.</p>
 *
 * <p>{@link Morphing} keeps every protocol in the cache under this hash. So
 * when an object changes, phino runs again only on the entries that reach
 * it, and not on every entry of the world.</p>
 *
 * @since 0.64.0
 */
final class Uses {

    /**
     * The top objects of the copies, with the hash of each copy and its
     * references.
     */
    private final Unchecked<Map<String, Map.Entry<String, Collection<String>>>> tops;

    /**
     * The XMIR of the entries.
     */
    private final Unchecked<XML> entries;

    /**
     * Ctor.
     *
     * @param home The home directory of the lowering
     */
    Uses(final Path home) {
        this(
            new Synced<>(new Sticky<>(new Tops(home))),
            new Synced<>(new Sticky<>(() -> new XMLDocument(home.resolve("entries.xmir"))))
        );
    }

    /**
     * Ctor.
     *
     * @param all The top objects of the copies
     * @param xmir The XMIR of the entries
     */
    Uses(
        final Scalar<Map<String, Map.Entry<String, Collection<String>>>> all,
        final Scalar<XML> xmir
    ) {
        this.tops = new Unchecked<>(all);
        this.entries = new Unchecked<>(xmir);
    }

    /**
     * The hash of the part of the world that one entry uses.
     *
     * @param number The number of the entry
     * @param loc The locator of the formation of the entry
     * @return The hash, in hex
     */
    String hash(final int number, final String loc) {
        final Map<String, Map.Entry<String, Collection<String>>> all = this.tops.value();
        final Collection<String> parts = new ArrayList<>(0);
        final Deque<String> todo = new ArrayDeque<>(0);
        todo.add(loc);
        for (final XML node : this.entries.value().nodes(
            String.format("/object/o/o[@name='e%d' or @name='mark' or @name='root']", number)
        )) {
            parts.add(node.toString());
            todo.addAll(node.xpath("descendant-or-self::o/@base[starts-with(., 'Φ.')]"));
        }
        final Collection<String> reached = new TreeSet<>();
        final Collection<String> outside = new TreeSet<>();
        while (!todo.isEmpty()) {
            final String ref = todo.pop();
            final String top = this.top(all, ref);
            if (top.isEmpty()) {
                outside.add(ref);
            } else if (reached.add(top)) {
                todo.addAll(all.get(top).getValue());
            }
        }
        for (final String top : reached) {
            parts.add(String.format("%s %s", top, all.get(top).getKey()));
        }
        parts.addAll(outside);
        return new UncheckedText(
            new HexOf(
                new Sha256DigestOf(
                    new InputOf(String.join("\n", parts).getBytes(StandardCharsets.UTF_8))
                )
            )
        ).asString();
    }

    /**
     * The locator of the top object that holds the object of a reference.
     *
     * @param all The top objects of the copies
     * @param ref The full locator of the object
     * @return The locator of its top object, or an empty string when no copy holds it
     */
    private String top(final Map<String, ?> all, final String ref) {
        String prefix = ref;
        while (!prefix.isEmpty() && !all.containsKey(prefix)) {
            prefix = prefix.substring(0, Math.max(prefix.lastIndexOf('.'), 0));
        }
        return prefix;
    }
}
