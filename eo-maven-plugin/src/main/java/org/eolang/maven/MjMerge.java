/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.util.Collection;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.list.ListOf;

/**
 * Put every member of a package inside the object that the package names.
 *
 * <p>An object and the package of the same name are two things today, and it
 * takes the runtime to join them: asked for an attribute it does not hold,
 * {@code Φ.number} goes looking for {@code Φ.number.lt} on the classpath and
 * binds itself into the first void of what it finds. This goal does that
 * joining at compile time instead, where the parts are already on disk as
 * XMIR, so that {@code lt} is simply an attribute of {@code number} and
 * nothing has to be searched for at runtime.</p>
 *
 * <p>It runs after {@link MjLint}, so that every member is read and reported
 * on as the file a human wrote, and before {@link MjTranspile}, which is the
 * first goal that cares about the shape of the object it compiles. The
 * merged XMIR goes to {@link Merging#DIR}.</p>
 *
 * <p>No package is merged unless it is named in {@code mergedPackages}, which
 * is empty by default, so a build that says nothing keeps the runtime
 * behaviour it has today.</p>
 *
 * @since 0.68.0
 */
@Mojo(
    name = "merge",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjMerge extends MjSafe {

    /**
     * The packages whose members are put inside the object of the same name,
     * as in {@code number}, one per element. A package that is not named here
     * is left alone, and its members keep being compiled as objects of their
     * own, found by the runtime.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(property = "eo.mergedPackages")
    @SuppressWarnings("PMD.ImmutableField")
    private Collection<String> mergedPackages = new ListOf<>();

    @Override
    void exec() throws IOException {
        try (TjsForeign tojos = this.tojos()) {
            new Timed(
                new Merging(
                    tojos,
                    this.targetDir.toPath().resolve(Merging.DIR),
                    this.mergedPackages
                )
            ).exec();
        }
    }
}
