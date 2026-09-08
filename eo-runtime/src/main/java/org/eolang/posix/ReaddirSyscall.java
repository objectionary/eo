/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.Platform;
import com.sun.jna.Pointer;
import org.eolang.Data;
import org.eolang.Handles;
import org.eolang.Int;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * Readdir syscall.
 *
 * <p>Takes the next entry out of an open directory stream and hands its name
 * to EO, with the code {@code 0}. The stream running out is a {@code NULL}
 * coming back, and then the code is {@code -1} and there is no name: EO reads
 * entries until it sees that, the way a C program does.</p>
 *
 * <p>Only the name is read out of the {@code struct dirent}, and the whole
 * struct is never mapped, since the fields ahead of {@code d_name} differ from
 * one platform to the next while the offset of the name itself is fixed on
 * each. Linux keeps the inode, the offset, the record length and the type
 * ahead of it, which is nineteen bytes; macOS keeps the inode, the seek
 * offset, the record length, the name length and the type, which is
 * twenty-one.</p>
 *
 * @since 0.76.0
 */
public final class ReaddirSyscall implements Syscall {

    /**
     * The offset of {@code d_name} inside {@code struct dirent}.
     */
    private static final long NAME;

    static {
        if (Platform.isMac()) {
            NAME = 21L;
        } else {
            NAME = 19L;
        }
    }

    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     *
     * @param posix Posix object
     */
    public ReaddirSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final String subject = "the 'dirp' argument of readdir";
        final Pointer stream = Handles.INSTANCE.get(
            subject, new Int(subject, params[0]).it()
        );
        final Phi result = this.posix.take("return").copy();
        final Pointer entry = CStdLib.INSTANCE.readdir(stream);
        final int code;
        final Phi output;
        if (entry == null) {
            code = -1;
            output = new PhDefault();
        } else {
            code = 0;
            output = new Data.ToPhi(entry.getString(ReaddirSyscall.NAME, "UTF-8"));
        }
        result.put(0, new Data.ToPhi(code));
        result.put(1, output);
        return result;
    }
}
