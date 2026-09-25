/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.Platform;
import com.sun.jna.Structure;
import java.util.function.ToIntBiFunction;
import org.eolang.Data;
import org.eolang.Phi;

/**
 * The status of one file, as a {@code posix.stat-return}.
 *
 * <p>Fills a {@code struct stat} for the file at the given path, through
 * {@code stat} or through {@code lstat} when a symbolic link has to be seen as
 * itself, and hands the code, the mode bits and the byte size to EO in one
 * object. Linux x86-64, Linux aarch64 and macOS lay that struct out
 * differently, so each keeps its own {@link FileStat}; the divergence is
 * spelled out rather than papered over.</p>
 *
 * @since 0.57.0
 */
public final class Stat {

    /**
     * The path of the file.
     */
    private final String path;

    /**
     * The C function filling the struct, either following a symbolic link or
     * reporting the link itself.
     */
    private final ToIntBiFunction<String, Structure> call;

    /**
     * Ctor.
     *
     * @param path The path of the file
     * @param call The C function filling the struct
     */
    public Stat(final String path, final ToIntBiFunction<String, Structure> call) {
        this.path = path;
        this.call = call;
    }

    /**
     * The status, filled.
     *
     * @return A copy of {@code posix.stat-return}
     */
    public Phi it() {
        final FileStat info;
        final int code;
        if (Platform.isMac()) {
            final MacFileStat mac = new MacFileStat();
            code = this.call.applyAsInt(this.path, mac);
            info = mac;
        } else if (Platform.isARM()) {
            final LinuxArmFileStat arm = new LinuxArmFileStat();
            code = this.call.applyAsInt(this.path, arm);
            info = arm;
        } else {
            final LinuxFileStat linux = new LinuxFileStat();
            code = this.call.applyAsInt(this.path, linux);
            info = linux;
        }
        final Phi result = Phi.Φ.take("posix").take("stat-return").copy();
        result.put(0, new Data.ToPhi(code));
        result.put(1, new Data.ToPhi(info.mode()));
        result.put(2, new Data.ToPhi(info.length()));
        return result;
    }

    /**
     * A file's status, exposing the two fields EO reads no matter how the
     * platform's {@code struct stat} is laid out.
     *
     * @since 0.74.0
     */
    public interface FileStat {

        /**
         * File mode bits, carrying the type and permissions.
         *
         * @return Mode bits, masked to sixteen bits
         */
        long mode();

        /**
         * File size.
         *
         * @return Size in bytes
         */
        long length();
    }
}
