/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsm; // NOPMD

import org.eolang.Phi;

/**
 * System call that can be made with EO objects ({@link Phi}) as arguments.
 *
 * @since 0.40
 */
@FunctionalInterface
public interface Syscall {
    /**
     * Makes native method call.
     *
     * @param params Native methods parameters.
     * @return Methods return code.
     */
    Phi make(Phi... params);
}
