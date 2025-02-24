/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.security.SecureRandom;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Random;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.maven.model.Dependency;

/**
 * Fake dependencies.
 *
 * @since 0.30
 */
final class DcsFake implements Iterable<Dependency> {

    /**
     * Dependencies.
     */
    private final Collection<Dependency> dependencies;

    /**
     * Ctor.
     */
    DcsFake() {
        this(
            DcsFake.randDep(),
            DcsFake.randDep(),
            DcsFake.randDep()
        );
    }

    /**
     * Ctor.
     * @param size Number of fake dependencies.
     */
    DcsFake(final int size) {
        this(Stream.generate(DcsFake::randDep).limit(size).collect(Collectors.toList()));
    }

    /**
     * Ctor.
     * @param deps Dependencies.
     */
    DcsFake(final Dependency... deps) {
        this(Arrays.asList(deps));
    }

    /**
     * Ctor.
     * @param deps Dependencies.
     */
    private DcsFake(final Collection<Dependency> deps) {
        this.dependencies = deps;
    }

    @Override
    public Iterator<Dependency> iterator() {
        return this.dependencies.iterator();
    }

    /**
     * Create a random dependency with specified scope.
     * @param scope Scope.
     * @return Dependency.
     */
    static Dependency randDep(final String scope) {
        final Random rand = new SecureRandom();
        return DcsFake.dep(
            UUID.randomUUID().toString(),
            UUID.randomUUID().toString(),
            String.valueOf(rand.nextInt(Integer.MAX_VALUE)),
            scope
        );
    }

    /**
     * Create a eo-runtime dependency.
     * @return Dependency.
     */
    static Dependency runtimeDep() {
        return DcsFake.dep(
            "org.eolang",
            "eo-runtime",
            "0.30.0",
            "compiled"
        );
    }

    /**
     * Create a random dependency.
     * @return Dependency.
     */
    private static Dependency randDep() {
        final Random rand = new SecureRandom();
        final String scope = new String[] {
            "test", "compiled", "runtime",
        }[rand.nextInt(3)];
        return DcsFake.dep(
            UUID.randomUUID().toString(),
            UUID.randomUUID().toString(),
            String.valueOf(rand.nextInt(Integer.MAX_VALUE)),
            scope
        );
    }

    /**
     * Create a dependency.
     * @param group Group ID.
     * @param artifact Artifact ID.
     * @param version Version.
     * @param scope Scope.
     * @return Dependency.
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    @SuppressWarnings("PMD.UseObjectForClearerAPI")
    private static Dependency dep(
        final String group,
        final String artifact,
        final String version,
        final String scope
    ) {
        final Dependency dependency = new Dependency();
        dependency.setGroupId(group);
        dependency.setArtifactId(artifact);
        dependency.setVersion(version);
        dependency.setScope(scope);
        return dependency;
    }
}
