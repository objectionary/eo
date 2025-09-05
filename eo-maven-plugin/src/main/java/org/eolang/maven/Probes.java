/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xml;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import java.io.FileNotFoundException;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.stream.Stream;
import org.cactoos.iterable.IterableOf;
import org.cactoos.iterable.Mapped;

/**
 * Possible objects.
 * This class represent a list of possible objects that were detected in
 * a XMIR file.
 *
 * @since 0.53
 */
final class Probes implements Iterable<String> {

    /**
     * Shift that adds probes.
     */
    private static final Shift ADD_PROBES = new StClasspath(
        "/org/eolang/maven/probe/add-probes.xsl"
    );

    /**
     * Xmir file where probes are searched.
     */
    private final XML xmir;

    /**
     * Constructor.
     * @param path Path to XMIR file
     * @throws FileNotFoundException If the file is not found
     */
    Probes(final Path path) throws FileNotFoundException {
        this(new XMLDocument(path));
    }

    /**
     * Constructor.
     * @param xmir XMIR file where probes are searched
     */
    Probes(final XML xmir) {
        this.xmir = xmir;
    }

    @Override
    public Iterator<String> iterator() {
        return new Mapped<>(Probes::noPrefix, this.metas()).iterator();
    }

    /**
     * Return metas for probing.
     * The equivalent xpath is:
     * "/object/metas/meta[head/text()='probe' or head/text()='also']/tail[not(text()='')]/text()"
     * @return Metas to probe
     */
    private Iterable<String> metas() {
        return new IterableOf<>(
            new Xnav(Probes.ADD_PROBES.apply(0, this.xmir).inner())
                .element("object")
                .elements(Filter.withName("metas"))
                .findFirst()
                .map(
                    all -> all.elements(Filter.all(Filter.withName("meta"), Probes::probeOrAlso))
                        .map(meta -> meta.element("tail").text().orElse(""))
                        .filter(meta -> !meta.isEmpty())
                )
                .orElse(Stream.of())
                .iterator()
        );
    }

    /**
     * Check if the meta is a 'probe' or 'also'.
     * @param meta Meta XML element
     * @return True if the meta is a 'probe' or 'also', false otherwise
     */
    private static Boolean probeOrAlso(final Xml meta) {
        return new Xnav(meta).element("head").text()
            .map(text -> "probe".equals(text) || "also".equals(text))
            .orElse(false);
    }

    /**
     * Trim Φ prefix.
     * Φ.a.b.c -> a.b.c
     * a.b.c -> a.b.c
     * @param obj Full object name
     * @return Trimmed object name
     */
    private static String noPrefix(final String obj) {
        final String result;
        if (obj.startsWith("Φ.")) {
            result = obj.substring(2);
        } else {
            result = obj;
        }
        return result;
    }
}
