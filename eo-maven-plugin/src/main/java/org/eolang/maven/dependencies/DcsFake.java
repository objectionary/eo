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
public final class DcsFake implements Iterable<Dependency> {

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
            String.valueOf(Math.abs(rand.nextInt())),
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
        return DcsFake.dep(
            UUID.randomUUID().toString(),
            UUID.randomUUID().toString(),
            String.valueOf(Math.abs(rand.nextInt())),
            new String[]{"test", "compiled", "runtime"}[rand.nextInt(3)]
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
