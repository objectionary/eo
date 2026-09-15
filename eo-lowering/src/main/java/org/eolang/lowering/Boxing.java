/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import java.nio.file.Path;

/**
 * The boxing of every formation of the build.
 *
 * <p>A box is a lambda planted next to the body of a formation, so that
 * entering that body is a fire the atom engine is asked to serve, and the
 * name of the box is how every stage after this one knows which formation
 * it is looking at. Nothing else of a file changes, which is what lets the
 * patch of a later stage take the boxes out again and arrive back at the
 * source the author wrote.</p>
 *
 * @since 0.74.0
 * @todo #8548:60min Copy every XMIR file of the build into the
 *  {@code boxed/} directory under the lowering directory, running
 *  {@code box.xsl} over each of them, which plants one
 *  {@code L_box_<n>_<carrier>} lambda on every formation that has a body
 *  and none at all on an atom, since an atom carries a lambda of its own
 *  already. The number is unique across the whole world and not merely
 *  within a file, so this stage hands each file an offset to count from,
 *  and the carrier is the type the {@code eo:inference} tables of
 *  {@code provides.xml} give the phi of the formation, or {@code object}
 *  when they give none. Until then neither the XMIR files of the build nor
 *  the directory of those tables are even passed in.
 */
final class Boxing implements Stage {

    /**
     * The directory where the lowering keeps what it makes.
     */
    private final Path home;

    /**
     * Ctor.
     *
     * @param dir The directory where the lowering keeps what it makes
     */
    Boxing(final Path dir) {
        this.home = dir;
    }

    @Override
    public void exec() {
        Logger.debug(this, "No formation is boxed yet in %s", this.home);
    }
}
