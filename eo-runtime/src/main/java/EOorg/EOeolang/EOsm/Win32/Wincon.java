/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsm.Win32; // NOPMD

/**
 * Ported from Wincon.h.
 * @since 0.40
 * @checkstyle InterfaceIsTypeCheck (5 lines)
 */
@SuppressWarnings("PMD.ConstantsInInterface")
public interface Wincon {
    /**
     * Standard input handle.
     */
    int STD_INPUT_HANDLE = -10;

    /**
     * Standard output handle.
     */
    int STD_OUTPUT_HANDLE = -11;
}
