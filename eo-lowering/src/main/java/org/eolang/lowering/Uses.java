/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeSet;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import org.cactoos.bytes.Sha256DigestOf;
import org.cactoos.io.InputOf;
import org.cactoos.map.MapEntry;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Synced;
import org.cactoos.scalar.Unchecked;
import org.cactoos.text.HexOf;
import org.cactoos.text.UncheckedText;

/**
 * The part of the world that one entry uses, as a hash.
 *
 * <p>phino meets, while it runs an entry, only the objects the entry refers
 * to, the objects those refer to, and so on. One top object refers to
 * another by its full locator, so this class follows such references from
 * the entry through the copies until nothing new is found. The hash is made
 * of the entry, the objects that wrap it, the hash of every copy reached,
 * and every reference no copy holds, since a copy that holds it later must
 * change the hash too. {@link Morphing} keeps every protocol in the cache
 * under this hash.</p>
 *
 * @since 0.64.0
 */
final class Uses {

    /**
     * The top objects of the copies, each with the hash of its copy and the
     * full locators it refers to.
     */
    private final Unchecked<Map<String, Map.Entry<String, Collection<String>>>> tops;

    /**
     * The XMIR of the entries.
     */
    private final Unchecked<XML> entries;

    /**
     * The lock every reader of the entries takes, since a DOM is not safe
     * even for reads from several threads at once.
     */
    private final Lock lock;

    /**
     * Ctor.
     *
     * @param home The home directory of the lowering
     */
    Uses(final Path home) {
        this.tops = new Unchecked<>(
            new Synced<>(
                new Sticky<>(
                    () -> {
                        final Map<String, Map.Entry<String, Collection<String>>> all =
                            new HashMap<>(0);
                        if (Files.exists(home.resolve("1-planting"))) {
                            for (final Path copy : new Copies(home)) {
                                final XML xmir = new XMLDocument(copy);
                                all.put(
                                    xmir.xpath("/object/o[1]/@loc").get(0),
                                    new MapEntry<>(
                                        new HexOf(new Sha256DigestOf(new InputOf(copy))).asString(),
                                        new TreeSet<>(xmir.xpath("//o/@base[starts-with(., 'Φ.')]"))
                                    )
                                );
                            }
                        }
                        return all;
                    }
                )
            )
        );
        this.entries = new Unchecked<>(
            new Synced<>(new Sticky<>(() -> new XMLDocument(home.resolve("entries.xmir"))))
        );
        this.lock = new ReentrantLock();
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
        this.lock.lock();
        try {
            for (final XML node : this.entries.value().nodes(
                String.format("/object/o/o[@name='e%d' or @name='mark' or @name='root']", number)
            )) {
                parts.add(node.toString());
                todo.addAll(node.xpath("descendant-or-self::o/@base[starts-with(., 'Φ.')]"));
            }
        } finally {
            this.lock.unlock();
        }
        final Collection<String> reached = new TreeSet<>();
        final Collection<String> outside = new TreeSet<>();
        while (!todo.isEmpty()) {
            final String ref = todo.pop();
            String top = ref;
            while (!top.isEmpty() && !all.containsKey(top)) {
                top = top.substring(0, Math.max(top.lastIndexOf('.'), 0));
            }
            if (top.isEmpty()) {
                outside.add(ref);
            } else if (reached.add(top)) {
                todo.addAll(all.get(top).getValue());
                parts.add(String.format("%s %s", top, all.get(top).getKey()));
            }
        }
        parts.addAll(outside);
        return new UncheckedText(
            new HexOf(
                new Sha256DigestOf(
                    new InputOf(String.join(" ", parts).getBytes(StandardCharsets.UTF_8))
                )
            )
        ).asString();
    }
}
