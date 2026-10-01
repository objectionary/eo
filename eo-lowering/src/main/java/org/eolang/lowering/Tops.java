/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import org.cactoos.Scalar;
import org.cactoos.bytes.Sha256DigestOf;
import org.cactoos.io.InputOf;
import org.cactoos.map.MapEntry;
import org.cactoos.text.HexOf;
import org.cactoos.text.UncheckedText;

/**
 * The top objects of the copies of the sources, by their locators.
 *
 * <p>Every top object comes with the hash of the copy it is written in, and
 * with every reference that copy makes to an object by its full locator,
 * which is a {@code @base} that starts with {@code Φ}. {@link Uses} follows
 * these references from one top object to another, so the copies are read
 * here only once, and not once for every entry.</p>
 *
 * @since 0.64.0
 */
final class Tops implements Scalar<Map<String, Map.Entry<String, Collection<String>>>> {

    /**
     * The home directory of the lowering, where the copies are.
     */
    private final Path home;

    /**
     * Ctor.
     *
     * @param dir The home directory of the lowering, where the copies are
     */
    Tops(final Path dir) {
        this.home = dir;
    }

    @Override
    public Map<String, Map.Entry<String, Collection<String>>> value() throws IOException {
        final Map<String, Map.Entry<String, Collection<String>>> tops = new HashMap<>(0);
        if (Files.exists(this.home.resolve("1-planting"))) {
            for (final Path copy : new Copies(this.home)) {
                final XML xmir = new XMLDocument(copy);
                tops.put(
                    xmir.xpath("/object/o[1]/@loc").get(0),
                    new MapEntry<>(
                        new UncheckedText(
                            new HexOf(new Sha256DigestOf(new InputOf(copy)))
                        ).asString(),
                        new HashSet<>(xmir.xpath("//o/@base[starts-with(., 'Φ.')]"))
                    )
                );
            }
        }
        return tops;
    }
}
