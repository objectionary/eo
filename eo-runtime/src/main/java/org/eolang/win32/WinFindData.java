/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.Structure;
import java.util.Arrays;
import java.util.List;

/**
 * The {@code WIN32_FIND_DATAW} a directory search fills in.
 *
 * <p>Only the name is ever read out of it, but the whole struct has to be
 * mapped anyway, since the kernel writes all of it and a short buffer would be
 * written past. The three file times are kept as six plain integers rather than
 * as {@code FILETIME} structures, because nothing here reads them and six
 * integers lay themselves out the same way. The name is a wide one, so it is a
 * {@code char[]}, which is how JNA spells {@code wchar_t[]}.</p>
 *
 * @since 0.76.0
 * @checkstyle VisibilityModifierCheck (60 lines)
 */
public final class WinFindData extends Structure {

    /**
     * Attribute bits of the entry.
     */
    public int attributes;

    /**
     * Creation, last access and last write times, two integers each.
     */
    public int[] times;

    /**
     * High half of the size in bytes.
     */
    public int high;

    /**
     * Low half of the size in bytes.
     */
    public int low;

    /**
     * Reparse tag and the word after it, which EO does not read.
     */
    public int[] reserved;

    /**
     * Name of the entry, ending at the first NUL.
     */
    public char[] name;

    /**
     * The 8.3 name of the entry, ending at the first NUL.
     */
    public char[] alternate;

    /**
     * Ctor.
     */
    public WinFindData() {
        super();
        this.times = new int[6];
        this.reserved = new int[2];
        this.name = new char[260];
        this.alternate = new char[14];
    }

    @Override
    public List<String> getFieldOrder() {
        return Arrays.asList(
            "attributes", "times", "high", "low", "reserved", "name", "alternate"
        );
    }

    /**
     * The name of the entry, as text.
     * @return The name, up to the NUL that ends it
     */
    public String filename() {
        int length = 0;
        while (length < this.name.length && this.name[length] != 0) {
            length += 1;
        }
        return new String(this.name, 0, length);
    }
}
