/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Platform;
import com.sun.jna.Pointer;
import org.eolang.posix.CStdLib;
import org.eolang.sys.Handles;

/**
 * Takes the next entry out of an open directory stream, as POSIX `readdir`
 * does.
 *
 * <p>The name comes with the code {@code 0}. The stream running out is a
 * {@code NULL} coming back, and then the code is {@code -1} and there is no
 * name: EO reads entries until it sees that, the way a C program does.</p>
 *
 * <p>Only the name is read out of the {@code struct dirent}, and the whole
 * struct is never mapped, since the fields ahead of {@code d_name} differ from
 * one platform to the next while the offset of the name itself is fixed on
 * each. Linux keeps the inode, the offset, the record length and the type
 * ahead of it, which is nineteen bytes; macOS keeps the inode, the seek
 * offset, the record length, the name length and the type, which is
 * twenty-one.</p>
 *
 * <p>The name is taken as the bytes it is, up to the first zero, and not as
 * text decoded from them. A name is any run of bytes the filesystem accepted,
 * and decoding one that is no UTF-8 gave back replacement characters, so the
 * name {@code directory.listed} answered with named no file any more
 * (#9049).</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.readdir")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOreaddir extends PhDefault implements Atom {

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
     * Ctor.
     */
    public EOposix$EOreaddir() {
        super(new Attrs(new Attr("dirp", new AtVoid("dirp"))));
    }

    @Override
    public Phi lambda() {
        final Pointer stream = Handles.INSTANCE.get(
            "the 'dirp' attribute",
            new Int(Expect.at(this, "dirp")).it()
        );
        final Phi result = Phi.Φ.take("posix").take("dir-return").copy();
        final Pointer entry = CStdLib.INSTANCE.readdir(stream);
        if (entry == null) {
            result.put(0, new Data.ToPhi(-1));
            result.put(1, new PhDefault());
        } else {
            final Phi name = Phi.Φ.take("string").copy();
            name.put(0, new Data.ToPhi(EOposix$EOreaddir.named(entry)));
            result.put(0, new Data.ToPhi(0));
            result.put(1, name);
        }
        return result;
    }

    /**
     * The bytes of {@code d_name}, up to the zero that closes it.
     *
     * @param entry The {@code struct dirent} libc handed back
     * @return The bytes of the name
     */
    private static byte[] named(final Pointer entry) {
        int length = 0;
        while (entry.getByte(EOposix$EOreaddir.NAME + length) != 0) {
            length += 1;
        }
        return entry.getByteArray(EOposix$EOreaddir.NAME, length);
    }
}
