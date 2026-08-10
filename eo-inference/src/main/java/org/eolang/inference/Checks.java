/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;

/**
 * What has to be checked later, and about which objects.
 *
 * <p>An application puts something into a copy of another object, which is a
 * promise that has to hold: in</p>
 *
 * <pre> &lt;o base="ξ.inc" loc="Φ.app.φ"&gt;
 *   &lt;o as="α0" base="ξ.t" loc="Φ.app.φ.α0"/&gt;
 * &lt;/o&gt;</pre>
 *
 * <p>the object at {@code Φ.app.φ.α0} has to fit into the first void of the
 * copy of {@code inc}. That is one row, and it is the last thing the tables
 * are missing: {@link Needs} knows that the void {@code x} is asked for
 * {@code next}, {@link Provides} knows what {@code t} has, {@link Links}
 * knows which is a copy of which, and this row is what makes those three
 * about the same question.</p>
 *
 * <p>Nothing is resolved here. Which void {@code α0} lands in depends on the
 * object being copied and on how many of its voids are already bound, and
 * both answers live in other tables — so the row keeps the binding as it was
 * written and leaves the arithmetic to the checking loop. A check recorded
 * without being decided is the whole point: the facts needed to decide it may
 * arrive later, or never.</p>
 *
 * @since 0.68.0
 */
final class Checks implements Clue {

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        final Collection<Row> rows = new ArrayList<>(0);
        for (final XML application : new Xmirs(xmirs).applications()) {
            final String owner = application.xpath("@loc").get(0);
            rows.add(new Row(owner));
            for (final XML argument : application.nodes("o[@as]")) {
                final String binding = argument.xpath("@as").get(0);
                rows.add(
                    new Row(String.join(" ", owner, binding))
                        .with("owner", owner)
                        .with("name", binding)
                        .with("type", argument.xpath("@loc").get(0))
                );
            }
        }
        Files.createDirectories(tables);
        Files.write(
            tables.resolve("checks.xml"),
            new Grouped(rows, "checks").asXml().toString().getBytes(StandardCharsets.UTF_8)
        );
    }
}
