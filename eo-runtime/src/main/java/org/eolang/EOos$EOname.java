/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Os.name.
 *
 * @since 0.40
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "os.name")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOos$EOname extends PhDefault implements Atom {

    /**
     * Operating system name as {@link Phi}.
     */
    private static final Phi OS_NAME = new Data.ToPhi(System.getProperty("os.name"));

    /**
     * Ctor.
     */
    public EOos$EOname() {
        // nothing
    }

    @Override
    public Phi lambda() {
        return EOos$EOname.OS_NAME;
    }
}
