/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collection;
import java.util.Iterator;
import org.apache.maven.model.Dependency;
import org.cactoos.Scalar;
import org.cactoos.list.ListOf;
import org.cactoos.scalar.Unchecked;

/**
 * Add runtime dependency to the list of dependencies, if it is absent there.
 * @since 0.28.11
 */
final class DpsWithRuntime implements Dependencies {

    /**
     * All dependencies.
     */
    private final Iterable<Dep> delegate;

    /**
     * Supplier of the eo-runtime dependency.
     */
    private final Unchecked<Dep> supplied;

    /**
     * Constructor.
     * @param dlg Dependencies delegate
     */
    DpsWithRuntime(final Iterable<Dep> dlg) {
        this(dlg, new Unchecked<>(new RtCentral()));
    }

    /**
     * Constructor.
     * @param dlg Dependencies delegate
     * @param sup Dependency
     */
    DpsWithRuntime(final Iterable<Dep> dlg, final Dep sup) {
        this(dlg, new Unchecked<>(() -> sup));
    }

    /**
     * The main constructor.
     * @param dlg Dependencies delegate
     * @param sup Supplier of the eo-runtime dependency
     */
    DpsWithRuntime(final Iterable<Dep> dlg, final Scalar<Dep> sup) {
        this(dlg, new Unchecked<>(sup));
    }

    /**
     * The main constructor.
     * @param dlg Dependencies delegate
     * @param sup Supplier of the eo-runtime dependency
     */
    DpsWithRuntime(final Iterable<Dep> dlg, final Unchecked<Dep> sup) {
        this.delegate = dlg;
        this.supplied = sup;
    }

    @Override
    public Iterator<Dep> iterator() {
        final Collection<Dep> all = new ListOf<>(this.delegate);
        if (all.stream().noneMatch(dep -> DpsWithRuntime.isRuntime(dep.get()))) {
            all.add(this.supplied.value());
        }
        return all.iterator();
    }

    /**
     * Is it our runtime dep?
     * @param other The dep
     * @return TRUE if it is
     */
    private static boolean isRuntime(final Dependency other) {
        return "org.eolang".equals(other.getGroupId())
            && "eo-runtime".equals(other.getArtifactId());
    }
}
