/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.List;

/**
 * One call at a place in a copy, with its arguments worked out (design doc
 * 4.1): each argument as worked out in the copy around, and the same
 * argument reduced (4.2).
 *
 * @param site The place of the call, under the tag of the copy around
 * @param ctx The copy around
 * @param raw The arguments, as worked out, in order
 * @param sets The arguments, reduced, in order
 * @since 0.76.0
 */
record Call(Site site, int ctx, List<int[]> raw, List<int[]> sets) {

    /**
     * How many arguments there are.
     *
     * @return The count
     */
    int size() {
        return this.raw.size();
    }

    /**
     * Whether an argument for one of the empty voids of a head has nothing
     * yet, the absence rule (4.1).
     *
     * @param free How many empty voids the head has
     * @return TRUE if one has
     */
    boolean lacking(final int free) {
        boolean found = false;
        for (int arg = 0; arg < Math.min(free, this.sets.size()); ++arg) {
            if (this.sets.get(arg).length == 0) {
                found = true;
                break;
            }
        }
        return found;
    }
}
