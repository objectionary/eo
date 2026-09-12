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
 * <p>Linux x86-64, Linux aarch64 and macOS order the fields of that struct
 * differently, and a 32-bit ARM kernel orders them differently again, while
 * mapping none of the three. Which of them this is gets decided when the
 * object is made, so a test can ask for the struct of a platform other than
 * the one it runs on.</p>
 *
 * @since 0.74.0
 */
final class StatLayout {

    /**
     * Whether this is macOS.
     */
    private final boolean mac;

    /**
     * Whether the architecture name is an ARM one, 32- or 64-bit.
     */
    private final boolean arm;

    /**
     * Whether the architecture is 64-bit.
     */
    private final boolean wide;

    /**
     * Ctor, for the platform this JVM runs on.
     */
    StatLayout() {
        this(Platform.isMac(), Platform.isARM(), Platform.is64Bit());
    }

    /**
     * Ctor.
     * @param mac Whether this is macOS
     * @param arm Whether the architecture name is an ARM one, 32- or 64-bit
     * @param wide Whether the architecture is 64-bit
     */
    StatLayout(final boolean mac, final boolean arm, final boolean wide) {
        this.mac = mac;
        this.arm = arm;
        this.wide = wide;
    }

    /**
     * An empty struct of this layout, for the C call to fill.
     * @param path The path being asked about, for the failure message
     * @return The struct, waiting to be filled
     */
    StatSyscall.FileStat stat(final String path) {
        if (!this.mac && this.arm && !this.wide) {
            throw new ExFailure(
                "Can't read the status of '%s': the 32-bit ARM 'struct stat' is not mapped yet",
                path
            );
        }
        final StatSyscall.FileStat info;
        if (this.mac) {
            info = new MacFileStat();
        } else if (this.arm) {
            info = new LinuxArmFileStat();
        } else {
            info = new LinuxFileStat();
        }
        return info;
    }
}
