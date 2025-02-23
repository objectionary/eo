/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.maven.model.Dependency;
import org.cactoos.list.ListOf;

/**
 * A decorator of a list of dependencies that throws an exception
 * if any dependency has a duplicate with a different version.
 *
 * @since 0.28.11
 */
final class DcsUniquelyVersioned implements Iterable<Dependency> {

    /**
     * Source of dependencies.
     */
    private final Iterable<Dependency> delegate;

    /**
     * The main constructor.
     *
     * @param dlg Source of dependencies.
     */
    DcsUniquelyVersioned(final Iterable<Dependency> dlg) {
        this.delegate = dlg;
    }

    @Override
    public Iterator<Dependency> iterator() {
        final Collection<Dependency> deps = new ListOf<>(this.delegate.iterator());
        final Map<String, Set<String>> conflicts = deps
            .stream()
            .collect(
                Collectors.groupingBy(
                    Dependency::getManagementKey,
                    Collectors.mapping(
                        Dependency::getVersion,
                        Collectors.toSet()
                    )
                )
            )
            .entrySet()
            .stream()
            .filter(e -> e.getValue().size() > 1)
            .collect(
                Collectors.toMap(
                    Map.Entry::getKey,
                    Map.Entry::getValue
                )
            );
        if (!conflicts.isEmpty()) {
            throw new IllegalStateException(
                String.format(
                    "%d conflicting dependencies are found: %s",
                    conflicts.size(),
                    conflicts
                )
            );
        }
        return deps.iterator();
    }
}
