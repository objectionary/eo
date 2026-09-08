/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

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
 * <p>Every package this build compiles an object for is merged. A package
 * whose name no object carries keeps its members as objects of their own,
 * reached through the package namespace as before.</p>
 *
 * <p>{@link MjCompile} runs the same step right after its lint, and
 * {@link MjTranspile} runs it again before it writes anything, so a build
 * that never names this goal still compiles merged objects, and every goal
 * between the two reads them merged. Naming it anyway costs nothing: a
 * member already inside its object is not moved again.</p>
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
     * Ctor.
     */
    public MjMerge() {
        // nothing
    }

    @Override
    void exec() throws IOException {
        try (TjsForeign tojos = this.tojos()) {
            new Timed(
                new Merging(
                    tojos,
                    this.targetDir.toPath().resolve(Merging.DIR)
                )
            ).exec();
        }
    }
}
