/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.Platform;
import org.eolang.ExFailure;

/**
 * The layout of {@code struct stat} on one platform.
 *
 * <p>Only three layouts are mapped: macOS, Linux aarch64 and Linux x86-64.
 * Any other architecture is refused rather than read through the nearest of
 * them. Thirty-two-bit ARM does not share the aarch64 struct, and RISC-V uses
 * the asm-generic one, where the mode sits where x86-64 keeps the link count,
 * so decoding either through a layout that is not its own answers with
 * whatever byte happens to lie at the offset.</p>
 *
 * <p>Which platform this is gets decided when the object is made, so a test
 * can ask for the struct of an architecture other than the one it runs on.</p>
 *
 * @since 0.74.0
 */
final class StatLayout {

    /**
     * The architecture, as {@link Platform#ARCH} spells it.
     */
    private final String arch;

    /**
     * Whether this is macOS.
     */
    private final boolean mac;

    /**
     * Ctor, for the platform this JVM runs on.
     */
    StatLayout() {
        this(Platform.ARCH, Platform.isMac());
    }

    /**
     * Ctor.
     *
     * @param arch The architecture, as {@link Platform#ARCH} spells it
     * @param mac Whether this is macOS
     */
    StatLayout(final String arch, final boolean mac) {
        this.arch = arch;
        this.mac = mac;
    }

    /**
     * An empty struct of this layout, for the C call to fill.
     *
     * @param path The path being asked about, for the failure message
     * @return The struct, waiting to be filled
     */
    Stat.FileStat stat(final String path) {
        final Stat.FileStat info;
        if (this.mac) {
            info = new MacFileStat();
        } else if ("aarch64".equals(this.arch)) {
            info = new LinuxArmFileStat();
        } else if ("x86-64".equals(this.arch)) {
            info = new LinuxFileStat();
        } else {
            throw new ExFailure(
                "Can't read the status of \"%s\", because no 'struct stat' is mapped for %s",
                path, this.arch
            );
        }
        return info;
    }
}
