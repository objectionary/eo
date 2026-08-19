/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

/**
 * Directory.deleted.entries.
 * @since 0.63
 * @checkstyle IllegalIdentifierNameCheck (30 lines)
 * @checkstyle TypeNameCheck (29 lines)
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
        final Collection<Phi> entries = new ArrayList<>(0);
        try {
            Collections.addAll(
                entries,
                new TupleToArray(
                    new PhApplication(dir.take("walk"), 0, new Data.ToPhi("**"))
                ).get()
            );
        } catch (final ExFailure ex) {
            if (new Dataized(dir.take("exists")).asBool()) {
                throw ex;
            }
        }
        return new Data.ToPhi(entries.toArray(new Phi[0]));
    }
}
