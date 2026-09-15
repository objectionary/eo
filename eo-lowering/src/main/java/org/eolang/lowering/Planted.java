/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.cactoos.Text;
import org.cactoos.text.Split;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;

/**
 * The boxes of a build, read out of its documents.
 *
 * <p>It takes every XMIR of the build and the formas of it. It answers one
 * box per named formation that declares arguments and stands under named
 * formations only, since that is the unit of lowering, each remembering
 * what it answers, what its receiver carries and the forma of every
 * argument. A stylesheet finds the formations, and the formas come from
 * the inference, so no Java here reads a document.</p>
 *
 * @since 0.77.0
 */
public final class Planted {

    /**
     * The XMIR documents.
     */
    private final List<Path> docs;

    /**
     * The formas of the build.
     */
    private final Formas formas;

    /**
     * Ctor.
     *
     * @param xmirs The XMIR documents
     * @param tables The formas of the build
     */
    public Planted(final List<Path> xmirs, final Formas tables) {
        this.docs = xmirs;
        this.formas = tables;
    }

    /**
     * All the boxes, in document order.
     *
     * @return The boxes
     * @throws IOException If a document cannot be read
     */
    public List<Box> all() throws IOException {
        final List<Box> out = new ArrayList<>(0);
        for (final Path doc : this.docs) {
            for (final Text line : new Split(new TextOf(Planted.planted(doc)), "\\R")) {
                final String row = new UncheckedText(line).asString();
                if (!row.isEmpty()) {
                    this.admitted(row.split("\t", -1), out);
                }
            }
        }
        return out;
    }

    private void admitted(final String[] cells, final List<Box> out) {
        if (!new Carrier(cells[0]).data()) {
            out.add(this.box(cells[0], !"0".equals(cells[1]), cells[2]));
        }
    }

    private Box box(final String place, final boolean reaches, final String voids) {
        final Map<String, String> row = new LinkedHashMap<>(0);
        row.put("locator", place);
        row.put("carrier", Planted.named(this.formas.at(place)));
        row.put("parent", Planted.receiver(place, reaches));
        row.put(
            "voids",
            Arrays.stream(voids.split(" ", -1)).filter(name -> !name.isEmpty()).map(
                name -> String.format(
                    "%s:%s", name, this.typed(String.format("%s.%s", place, name))
                )
            ).collect(Collectors.joining(" "))
        );
        return new Box(row);
    }

    private String typed(final String place) {
        return Planted.named(this.formas.known(place));
    }

    private static String receiver(final String place, final boolean reaches) {
        String out = "-";
        if (reaches) {
            out = Planted.named(
                new Carrier(place.substring(0, place.lastIndexOf('.'))).forma()
            );
        }
        return out;
    }

    private static String named(final String forma) {
        String out = forma;
        if (out.isEmpty()) {
            out = "object";
        }
        return out;
    }

    private static String planted(final Path doc) throws IOException {
        return new XSLDocument(
            Planted.class.getResource("/org/eolang/lowering/planting.xsl"), "planting.xsl"
        ).applyTo(new XMLDocument(doc));
    }
}
