/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven.dependencies;

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
public final class DcsUniquelyVersioned implements Iterable<Dependency> {

    /**
     * Source of dependencies.
     */
    private final Iterable<Dependency> delegate;

    /**
     * The main constructor.
     *
     * @param dlg Source of dependencies.
     */
    public DcsUniquelyVersioned(final Iterable<Dependency> dlg) {
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
