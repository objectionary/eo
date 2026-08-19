/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Directory.deleted.entries.
 * @since 0.63
 * @checkstyle IllegalIdentifierNameCheck (27 lines)
 * @checkstyle TypeNameCheck (26 lines)
 */
@XmirObject(oname = "directory.deleted.entries")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOdirectory$EOdeleted$EOentries extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOdirectory$EOdeleted$EOentries() {
        // nothing
    }

    @Override
    public Phi lambda() {
        final Phi dir = this.take(Phi.RHO).take("d");
        final Phi walked = new PhApplication(dir.take("walk"), 0, new Data.ToPhi("**"));
        final Phi entries;
        try {
            entries = new Data.ToPhi(new TupleToArray(walked).get());
        } catch (final ExFailure ex) {
            if (new Dataized(dir.take("exists")).asBool()) {
                throw ex;
            }
            return new Data.ToPhi(new Phi[0]);
        }
        return entries;
    }
}
