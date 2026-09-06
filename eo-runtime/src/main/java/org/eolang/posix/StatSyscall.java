/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.Structure;
import java.util.function.ToIntBiFunction;
import org.eolang.Cstring;
import org.eolang.Data;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * Stat syscall.
 *
 * <p>Fills a {@code struct stat} for the file at the given path, through
 * {@code stat} or through {@code lstat} when a symbolic link has to be seen as
 * itself, and hands its mode bits and byte size to EO. Linux x86-64, Linux
 * aarch64 and macOS lay that struct out differently, so each keeps its own
 * {@link FileStat}; the divergence is spelled out rather than papered over.
 * An architecture whose layout is not mapped fails loudly instead of reading
 * the fields of another ABI.</p>
 *
 * @since 0.57.0
 */
public final class StatSyscall implements Syscall {

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * The C function filling the struct, either following a symbolic link or
     * reporting the link itself.
     */
    private final ToIntBiFunction<String, Structure> call;

    /**
     * Ctor.
     * @param posix Posix object
     * @param call The C function filling the struct
     */
    public StatSyscall(final Phi posix, final ToIntBiFunction<String, Structure> call) {
        this.posix = posix;
        this.call = call;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.posix.take("return").copy();
        final String path = new Cstring("the 'path' argument of stat", params[0]).it();
        final FileStat info = new StatLayout().stat(path);
        result.put(0, new Data.ToPhi(this.call.applyAsInt(path, (Structure) info)));
        final Phi struct = this.posix.take("stat");
        struct.put(0, new Data.ToPhi(info.mode()));
        struct.put(1, new Data.ToPhi(info.length()));
        result.put(1, struct);
        return result;
    }

    /**
     * A file's status, exposing the two fields EO reads no matter how the
     * platform's {@code struct stat} is laid out.
     * @since 0.74.0
     */
    public interface FileStat {

        /**
         * File mode bits, carrying the type and permissions.
         * @return Mode bits, masked to sixteen bits
         */
        long mode();

        /**
         * File size.
         * @return Size in bytes
         */
        long length();
    }
}
