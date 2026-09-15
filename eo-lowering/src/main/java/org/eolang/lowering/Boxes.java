/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.tojos.MnTabs;
import com.yegor256.tojos.Mono;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

/**
 * The boxes of one build, kept in a file of tojos.
 *
 * <p>It takes the path of the file. It answers the box of a λ name and
 * all the rows at once, and it writes a new set of rows back. A row says
 * what a formation answers, what its receiver carries and the forma of
 * every argument — the facts the build learns once and the engine, in its
 * own process, cannot work out again.</p>
 *
 * @since 0.76.0
 */
public final class Boxes {

    /**
     * The rows.
     */
    private final Mono rows;

    /**
     * Ctor.
     *
     * @param table The file
     */
    public Boxes(final Path table) {
        this(new MnTabs(table));
    }

    /**
     * Ctor.
     *
     * @param storage The rows
     */
    Boxes(final Mono storage) {
        this.rows = storage;
    }

    /**
     * The box of a λ name.
     *
     * @param lambda The name, such as {@code L_box_p__foo__f}
     * @return The box
     */
    public Box at(final String lambda) {
        final String place = new Place(lambda).name();
        return this.all().stream()
            .filter(box -> box.locator().equals(place))
            .findFirst().orElseThrow(
                () -> new IllegalStateException(
                    String.format("No box is planted under the name '%s'", lambda)
                )
            );
    }

    /**
     * All boxes.
     *
     * @return The boxes, in the order planted
     */
    public List<Box> all() {
        return this.rows.read().stream().map(Box::new).collect(Collectors.toList());
    }

    /**
     * Write all boxes.
     *
     * @param boxes The boxes
     */
    public void save(final List<Box> boxes) {
        this.rows.write(boxes.stream().map(Box::row).collect(Collectors.toList()));
    }
}
