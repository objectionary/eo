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
 * differently, and an architecture that is none of the three, RISC-V among
 * them, orders them differently again while mapping none of them. Which of
 * them this is gets decided when the object is made, so a test can ask for
 * the struct of a platform other than the one it runs on.</p>
 *
 * @since 0.74.0
 */
final class StatLayout {

    /**
     * Whether this is macOS.
     */
    private final boolean mac;

    /**
     * Whether the architecture is an ARM one.
     */
    private final boolean arm;

    /**
     * Whether the architecture is an x86 one.
     */
    private final boolean intel;

    /**
     * Ctor, for the platform this JVM runs on.
     */
    StatLayout() {
        this(Platform.isMac(), Platform.isARM(), Platform.isIntel());
    }

    /**
     * Ctor.
     * @param mac Whether this is macOS
     * @param arm Whether the architecture is an ARM one
     * @param intel Whether the architecture is an x86 one
     */
    StatLayout(final boolean mac, final boolean arm, final boolean intel) {
        this.mac = mac;
        this.arm = arm;
        this.intel = intel;
    }

    /**
     * An empty struct of this layout, for the C call to fill.
     * @param path The path being asked about, for the failure message
     * @return The struct, waiting to be filled
     */
    StatSyscall.FileStat stat(final String path) {
        if (!this.mac && !this.arm && !this.intel) {
            throw new ExFailure(
                "Can't read the status of '%s': no 'struct stat' is mapped for the '%s' architecture",
                path, Platform.ARCH
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
