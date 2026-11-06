/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.nio.file.Path;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.Xtory;

/**
 * A test pack of this module, read from its YAML.
 *
 * <p>A pack carries the EO sources of a small program under {@code sources},
 * the XPaths its {@code answers.xml} must match under {@code answers}, and,
 * when its rule is about the facts, the XPaths its {@code facts.xml} must
 * match under {@code facts}. The comment at the top of the pack says what
 * the program shows, and each XPath checks one claim of that comment.</p>
 *
 * @since 0.76.0
 */
final class Pack {

    /**
     * The pack, read from its YAML.
     */
    private final Xtory story;

    /**
     * Ctor.
     *
     * @param yaml The YAML of the pack
     */
    Pack(final String yaml) {
        this(new XtSticky(new XtYaml(yaml)));
    }

    /**
     * Ctor.
     *
     * @param xtory The pack, read from its YAML
     */
    private Pack(final Xtory xtory) {
        this.story = xtory;
    }

    /**
     * The sources of the program.
     *
     * @param home The directory to make the facts in
     * @return The sources
     */
    Sources sources(final Path home) {
        return new Sources(
            home,
            ((Map<?, ?>) this.story.map().get("sources")).entrySet().stream().collect(
                Collectors.toMap(
                    entry -> entry.getKey().toString(),
                    entry -> entry.getValue().toString(),
                    (left, right) -> left,
                    LinkedHashMap::new
                )
            )
        );
    }

    /**
     * The XPaths the facts of the program must match.
     *
     * @return The XPaths, none when the rule of the pack is not about the facts
     */
    String[] facts() {
        return this.xpaths("facts");
    }

    /**
     * The XPaths the answers of the program must match.
     *
     * @return The XPaths, at least one
     */
    String[] answers() {
        final String[] xpaths = this.xpaths("answers");
        if (xpaths.length == 0) {
            throw new IllegalStateException("The pack has no XPath to check its answers with");
        }
        return xpaths;
    }

    private String[] xpaths(final String table) {
        return ((Collection<?>) this.story.map().getOrDefault(table, Collections.emptyList()))
            .stream()
            .map(Object::toString)
            .toArray(String[]::new);
    }
}
