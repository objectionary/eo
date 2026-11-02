/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * The tally of the inference report, a line per number.
 *
 * <p>A build that only logs the tally leaves nothing behind: the line scrolls
 * past and the next branch has nothing to be measured against. So the same
 * numbers go into {@code ladder.txt}, a line each, the value first and its
 * name after, a shape a shell reads with one {@code read} and no knowledge of
 * what any of it means. The count of every object comes first, the share of
 * each band out of a hundred after it, and the count of each band last, since
 * a share on its own is a number to game and the counts make the gaming
 * visible.</p>
 *
 * @since 0.76.0
 */
final class Ladder implements Iterable<String> {

    /**
     * How many objects fall in each band, in the order a tally lists them.
     */
    private final Map<String, Integer> tally;

    /**
     * Ctor.
     *
     * @param bands How many objects fall in each band
     */
    Ladder(final Map<String, Integer> bands) {
        this.tally = bands;
    }

    @Override
    public Iterator<String> iterator() {
        final int total = this.tally.values().stream().mapToInt(Integer::intValue).sum();
        final List<String> lines = new ArrayList<>(2 * this.tally.size() + 1);
        lines.add(String.format(Locale.ROOT, "%d objects in all", total));
        for (final Map.Entry<String, Integer> band : this.tally.entrySet()) {
            lines.add(
                String.format(
                    Locale.ROOT, "%.1f %% in %s",
                    100.0d * band.getValue() / Math.max(total, 1), band.getKey()
                )
            );
        }
        for (final Map.Entry<String, Integer> band : this.tally.entrySet()) {
            lines.add(
                String.format(Locale.ROOT, "%d objects in %s", band.getValue(), band.getKey())
            );
        }
        return lines.iterator();
    }
}
